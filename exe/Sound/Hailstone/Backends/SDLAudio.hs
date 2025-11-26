{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module Sound.Hailstone.Backends.SDLAudio
( HailstoneAudioHandle
, withAudio
, enableAudio
, putNode
, lockAudio
, resumeAudio
)
where

-- SDL backend specific
import qualified SDL as SDL
import qualified Data.Vector.Storable.Mutable as SMV

import System.Mem (performGC)
import Data.Word (Word16)
import Control.Concurrent
import qualified Sound.Hailstone.Backends.Common as Common
import Sound.Hailstone.Synth.Node

data HailstoneAudioHandle e = MkHAH
  { _HAHsdlAudioDevice :: SDL.AudioDevice
  -- ^ SDL audio device
  , _HAHchanMode :: ChanMode
  -- ^ mono or stereo
  , _HAHsampleRate :: SampleRate
  -- ^ sample rate
  , _HAHproducerThreadId :: ThreadId
  -- ^ thread ID of the producer thread
  , _HAHreplacementSinkMV :: MVar (Sink e)
  -- ^ comms channel to give the producer thread a new sink to play
  }

-- | callback that SDL will fire to fill a buffer of samples to play
sdlAudioCallback  :: ChanMode -- ^ stereo or mono
                  -> Common.SampleQueue
                  -> SDL.AudioFormat s -- ^SDL Audio Format identifier
                  -> SMV.IOVector s -- ^SDL-side vector to write samples to
                  -> IO ()
sdlAudioCallback cm sampQ sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  -- need to match on this because it's a GADT constructor that instantiates a sample type s
  SDL.Signed16BitLEAudio -> let
    realBufLen = SMV.length sdlAudioBuffer
    writer = SMV.write sdlAudioBuffer
    in Common.consumerLoop realBufLen cm sampQ () writer
  _ -> putStrLn "Unsupported audio sample format"

-- | Initialize SDL (the Audio subsystem only) given a sample rate, buffer size, and initial
-- node graph. Must do `closeAudio` afterwards; we expose `withAudio` which closes for you.
openAudio :: SampleRate -> Word16 -> ChanMode -> e -> Node e (LR SampleVal) -> IO (HailstoneAudioHandle e)
openAudio sampleRate nSamplesPerBuffer chanMode initEnv initNode = do
  SDL.initialize [SDL.InitAudio]
  let sampleType = SDL.Signed16BitNativeAudio
      stereoMode = case chanMode of
        Mono -> SDL.Mono
        Stereo -> SDL.Stereo
      fresh = initSink sampleRate initEnv
      sink = fresh { _destNode = initNode }

  -- start the producer thread
  (sampQ, replacementSinkMV, producerThreadId) <-
    Common.startProducer nSamplesPerBuffer chanMode sink

  -- function "requests" an audio spec from the hardware, using the requests/demands we
  -- specify in the Spec. It returns a spec, that has the true values that were provided,
  -- which hopefully matches our demands but might not exactly match our demands. (the spec
  -- is the 2nd tuple item, but that is unused)
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate $ fromIntegral sampleRate
    , SDL.openDeviceFormat = SDL.Mandate sampleType
    , SDL.openDeviceChannels = SDL.Mandate stereoMode
    , SDL.openDeviceSamples = nSamplesPerBuffer
    , SDL.openDeviceCallback = sdlAudioCallback chanMode sampQ
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
closeAudio :: HailstoneAudioHandle e -> IO ()
closeAudio hah = SDL.closeAudioDevice (_HAHsdlAudioDevice hah)
  *> killThread (_HAHproducerThreadId hah)

-- | Run SDL audio given a sample rate, buffer size, channel mode, extra signal environment,
-- initial node graph, and an IO action, and then terminate.
withAudio :: SampleRate  -- ^sample rate
          -> Word16 -- ^number of samples per buffer
          -> ChanMode -- ^stereo or mono
          -> e -- ^extra signal environment
          -> Node e (LR SampleVal) -- ^node graph
          -> (HailstoneAudioHandle e -> IO ()) -- ^IO actions given the handle (e.g. `enableAudio`)
          -> IO ()
withAudio sampleRate nSamplesPerBuffer chanMode initEnv initNode action =
  bracket (openAudio sampleRate nSamplesPerBuffer chanMode initEnv initNode) action closeAudio

-- | Play a new node graph.
putNode :: e -> Node e (LR SampleVal) -> HailstoneAudioHandle e -> IO ()
putNode newEnv newNode hah = do
  -- update the sample source (and reset the time counter), but don't change
  -- anything about the delta value
  let fresh = initSink (_HAHsampleRate hah) newEnv
      newSink = fresh { _destNode = newNode }
      replacementSinkMV = _HAHreplacementSinkMV hah
  putMVar replacementSinkMV newSink

-- | Enable SDL audio, beginning playback. (gc first then do it)
enableAudio :: HailstoneAudioHandle e -> IO ()
enableAudio hah = performGC *> SDL.setAudioDevicePlaybackState (_HAHsdlAudioDevice hah) SDL.Play

-- | Pauses SDL audio. (This prevents the SDL audio callback from firing.)
lockAudio :: HailstoneAudioHandle e -> IO ()
lockAudio hah = SDL.setAudioDeviceLocked (_HAHsdlAudioDevice hah) SDL.Locked

-- | Resumes SDL audio.
resumeAudio :: HailstoneAudioHandle e -> IO ()
resumeAudio hah = SDL.setAudioDeviceLocked (_HAHsdlAudioDevice hah) SDL.Unlocked
