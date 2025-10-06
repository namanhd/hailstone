{-# LANGUAGE GADTs, BangPatterns, MagicHash, UnboxedTuples #-}

module Sound.Hailstone.Backends.SDLAudio
( HailstoneAudioHandle
, openAudio
, enableAudio
, putNode
, lockAudio
, resumeAudio
, closeAudio
)
where

import Control.Concurrent
import qualified Control.Concurrent.Chan.Unagi.Unboxed as UU  -- concurrent FIFO channel
import qualified SDL as SDL
import Data.Word (Word16) -- for buffer size
import qualified Data.Vector.Storable.Mutable as SMV
import System.Mem
import qualified GHC.Base as GHCB
import GHC.Exts (tryReadMVar#, tryTakeMVar#)
import qualified GHC.MVar as GHCMV

import Sound.Hailstone.Synth

type SampleChIn = UU.InChan SampleVal
type SampleChOut = UU.OutChan SampleVal

data HailstoneAudioHandle = MkHAH
  { _HAHsdlAudioDevice :: SDL.AudioDevice
  -- ^ SDL audio device
  , _HAHchanMode :: ChanMode
  -- ^ mono or stereo
  , _HAHsampleRate :: SampleRate
  -- ^ sample rate
  , _HAHproducerThreadId :: ThreadId
  -- ^ thread ID of the producer thread
  , _HAHreplacementSinkMV :: MVar Sink
  -- ^ comms channel to give the producer thread a new sink to play
  }

-- | these should strictly allocate less than `tryReadMVar` which returns IO (Maybe a) but
-- somehow they have weirdly different deadlock behavior in the sdl callback when subbed in
-- the same place with the same usage...???
tryReadMVarWithDefault :: GHCMV.MVar a -> a -> IO a
tryReadMVarWithDefault (GHCMV.MVar m) dflt = GHCB.IO $ \s -> case tryReadMVar# m s of
  (# s', 0#, _ #) -> (# s', dflt #)
  (# s', _, !a #) -> (# s', a #)

tryTakeMVarWithDefault :: GHCMV.MVar a -> a -> IO a
tryTakeMVarWithDefault (GHCMV.MVar m) dflt = GHCB.IO $ \s -> case tryTakeMVar# m s of
  (# s', 0#, _ #) -> (# s', dflt #)
  (# s', _, !a #) -> (# s', a #)

-- | loop body for producer thread
producerLoop :: ChanMode -> SampleChIn -> MVar Int -> MVar Sink -> Sink -> IO ()
producerLoop cm sampChI queuedCountMV replacementSinkMV sink = do
  let maxQueue = 256  -- make this configurable?
  -- this value can be as large as we want for song playback but it'll need to be small to
  -- respond to realtime inputs if that ever becomes a thing
  !sinkToPlayNow <- tryTakeMVarWithDefault replacementSinkMV sink
  !qc' <- tryReadMVarWithDefault queuedCountMV maxQueue
  if qc' >= maxQueue
  -- stall until qc' goes under this cap again
  then producerLoop cm sampChI queuedCountMV replacementSinkMV sinkToPlayNow
  else do
    let !node = _destNode sinkToPlayNow
        !r@(!t, !d) = _sigEnv sinkToPlayNow
        (MkLR !xl !xr, !nextNode) = runNode r node
        !nextSink = MkSink { _destNode = nextNode, _sigEnv = (t + d, d) }
    -- write samples to the channel
    case cm of
      Stereo -> do
        modifyMVar_ queuedCountMV $ \(!qc) -> UU.writeChan sampChI xl *> UU.writeChan sampChI xr *> pure (qc + 2)
      Mono -> do
        modifyMVar_ queuedCountMV $ \(!qc) -> UU.writeChan sampChI (xl + xr) *> pure (qc + 1)
    -- loop
    producerLoop cm sampChI queuedCountMV replacementSinkMV nextSink


-- | Start the producer thread which will loop forever and write to the sample chan.
startProducer :: ChanMode -> Sink -> IO (SampleChOut, MVar Int, MVar Sink, ThreadId)
startProducer cm sink = do
  -- comms channel for the producer to give us samples to copy to the SDL buffer
  (sampChI, sampChO) <- UU.newChan  -- seems like a good number, hovering at 24MiB memory usage with this
  -- comms channel for us to give the producer new replacement sinks to play
  replacementSinkMV <- newEmptyMVar
  -- comms channel to limit the number of samples in the queue
  queuedCountMV <- newMVar 0
  -- make the producer thread
  threadId <- forkIO $ producerLoop cm sampChI queuedCountMV replacementSinkMV sink
  pure (sampChO, queuedCountMV, replacementSinkMV, threadId)

-- | callback that SDL will fire to fill a buffer of samples to play
sdlAudioCallback  :: ChanMode -- ^ stereo or mono
                  -> SampleChOut -- ^the output end of the sample chan/queue, to consume
                  -> MVar Int -- ^queued count
                  -> SDL.AudioFormat s -- ^SDL Audio Format identifier
                  -> SMV.IOVector s -- ^SDL-side vector to write samples to
                  -> IO ()
sdlAudioCallback cm sampChO queuedCountMV sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  -- need to match on this because it's a GADT constructor that instantiates a sample type s
  SDL.Signed16BitLEAudio -> loop 0
    where
      bufferSize = SMV.length sdlAudioBuffer
      writeit ii = UU.readChan sampChO >>= SMV.write sdlAudioBuffer ii
      -- loop thru indices i and fill the buffer
      loop i = if i >= bufferSize then pure () else do
        !mqc <- tryReadMVar queuedCountMV  -- CANNOT use my tryReadMVarWithDefault here without deadlock???
        if (case mqc of Just !qqc -> qqc; _ -> 0) <= 0 then loop i else case cm of
          Stereo -> do
            modifyMVar_ queuedCountMV $ \(!qc) -> writeit i *> writeit (i + 1) *> pure (qc - 2)
            loop (i + 2)
          Mono -> do
            modifyMVar_ queuedCountMV $ \(!qc) -> writeit i *> pure (qc - 1)
            loop (i + 1)
  _ -> putStrLn "Unsupported audio sample format"

-- | Initialize SDL (the Audio subsystem only) given a sample rate, buffer size, and initial
-- node graph
openAudio :: SampleRate -> Word16 -> ChanMode -> Node (LR SampleVal) -> IO HailstoneAudioHandle
openAudio sampleRate bufferSize chanMode initNode = do
  SDL.initialize [SDL.InitAudio]
  let sampleType = SDL.Signed16BitNativeAudio
      stereoMode = case chanMode of
        Mono -> SDL.Mono
        Stereo -> SDL.Stereo
      fresh = initSink sampleRate
      sink = fresh { _destNode = initNode }

  -- start the producer thread
  (sampChO, queuedCountMV, replacementSinkMV, producerThreadId) <- startProducer chanMode sink

  -- function "requests" an audio spec from the hardware, using the requests/demands we
  -- specify in the Spec. It returns a spec, that has the true values that were provided,
  -- which hopefully matches our demands but might not exactly match our demands. (the spec
  -- is the 2nd tuple item, but that is unused)
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate $ fromIntegral sampleRate
    , SDL.openDeviceFormat = SDL.Mandate sampleType
    , SDL.openDeviceChannels = SDL.Mandate stereoMode
    , SDL.openDeviceSamples = bufferSize
    , SDL.openDeviceCallback = sdlAudioCallback chanMode sampChO queuedCountMV
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing -- any output audio device will do
    }
  -- TODO we should grab (SDL.audioSpecSilence spec) and somehow tell the Synth
  -- module that that value is what represents silence (for now we default to 0)

  -- maybe this helps to reduce initial stutters by preemptively GC'ing (we also do this at
  -- the start of enableAudio)
  performGC
  pure $ MkHAH
    { _HAHsdlAudioDevice = device
    , _HAHchanMode = chanMode
    , _HAHsampleRate = sampleRate
    , _HAHproducerThreadId = producerThreadId
    , _HAHreplacementSinkMV = replacementSinkMV
    }

-- | Close SDL audio. Should be called at the end of main. Also kills the producer thread.
closeAudio :: HailstoneAudioHandle -> IO ()
closeAudio hah = SDL.closeAudioDevice (_HAHsdlAudioDevice hah)
  *> killThread (_HAHproducerThreadId hah)

-- | Play a new node graph.
putNode :: HailstoneAudioHandle -> Node (LR SampleVal) -> IO ()
putNode hah newNode = do
  -- update the sample source (and reset the time counter), but don't change
  -- anything about the delta value
  let fresh = initSink $ _HAHsampleRate hah
      newSink = fresh { _destNode = newNode }
      replacementSinkMV = _HAHreplacementSinkMV hah
  putMVar replacementSinkMV newSink

-- | Enable SDL audio, beginning playback. (gc first then do it)
enableAudio :: HailstoneAudioHandle -> IO ()
enableAudio hah = performGC *> SDL.setAudioDevicePlaybackState (_HAHsdlAudioDevice hah) SDL.Play

-- | Pauses SDL audio. (This prevents the SDL audio callback from firing.)
lockAudio :: HailstoneAudioHandle -> IO ()
lockAudio hah = SDL.setAudioDeviceLocked (_HAHsdlAudioDevice hah) SDL.Locked

-- | Resumes SDL audio.
resumeAudio :: HailstoneAudioHandle -> IO ()
resumeAudio hah = SDL.setAudioDeviceLocked (_HAHsdlAudioDevice hah) SDL.Unlocked
