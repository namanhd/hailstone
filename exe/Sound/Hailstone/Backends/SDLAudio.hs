{-# LANGUAGE GADTs, BangPatterns #-}

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
import qualified Control.Concurrent.Chan.Unagi.Bounded as UB  -- concurrent FIFO channel
import qualified SDL as SDL
import Data.Word (Word16) -- for buffer size
import qualified Data.Vector.Storable.Mutable as SMV
import System.Mem

import Sound.Hailstone.Synth

type SampleChIn = UB.InChan SampleVal
type SampleChOut = UB.OutChan SampleVal

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

producerLoop :: ChanMode -> SampleChIn -> MVar Sink -> Sink -> IO ()
producerLoop cm sampChI replacementSinkMV sink = do
  !maybeReplacementSink <- tryTakeMVar replacementSinkMV
  let !sinkToPlayNow = maybe sink id maybeReplacementSink
      !node = _destNode sinkToPlayNow
      !r@(t, d) = _sigEnv sinkToPlayNow
      (MkLR (!xl, !xr), !nextNode) = runNode r node
      !nextSink = MkSink { _destNode = nextNode, _sigEnv = (t + d, d) }
  -- write samples to the channel
  case cm of
    Stereo -> UB.writeChan sampChI xl *> UB.writeChan sampChI xr
    Mono -> UB.writeChan sampChI (xl + xr)
  -- loop
  nextSink `seq` producerLoop cm sampChI replacementSinkMV nextSink

-- | Start the producer thread which will loop forever and write to the sample chan.
startProducer :: ChanMode -> Sink -> IO (SampleChOut, MVar Sink, ThreadId)
startProducer cm sink = do
  -- comms channel for the producer to give us samples to copy to the SDL buffer
  (sampChI, sampChO) <- UB.newChan 16384  -- seems like a good number, hovering at 24MiB memory usage with this
  -- comms channel for us to give the producer new replacement sinks to play
  replacementSinkMV <- newEmptyMVar
  -- make the producer thread
  threadId <- forkIO $ producerLoop cm sampChI replacementSinkMV sink
  pure (sampChO, replacementSinkMV, threadId)

sdlAudioCallback  :: SampleChOut -- ^the output end of the sample chan/queue, to consume
                  -> SDL.AudioFormat s -- ^SDL Audio Format identifier
                  -> SMV.IOVector s -- ^SDL-side vector to write samples to
                  -> IO ()
sdlAudioCallback sampChO sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  SDL.Signed16BitLEAudio -> let -- turns out we need GADTs turned on for this
    bufferSize = SMV.length sdlAudioBuffer
    in mapM_ (\i -> UB.readChan sampChO >>= SMV.write sdlAudioBuffer i) [0..(bufferSize - 1)]
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
  (sampChO, replacementSinkMV, producerThreadId) <- startProducer chanMode sink

  -- function "requests" an audio spec from the hardware, using the requests/demands we
  -- specify in the Spec. It returns a spec, that has the true values that were provided,
  -- which hopefully matches our demands but might not exactly match our demands. (the spec
  -- is the 2nd tuple item, but that is unused)
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate $ fromIntegral sampleRate
    , SDL.openDeviceFormat = SDL.Mandate sampleType
    , SDL.openDeviceChannels = SDL.Mandate stereoMode
    , SDL.openDeviceSamples = bufferSize
    , SDL.openDeviceCallback = sdlAudioCallback sampChO
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
