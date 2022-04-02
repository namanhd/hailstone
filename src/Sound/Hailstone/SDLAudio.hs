{-# LANGUAGE GADTs #-}

module Sound.Hailstone.SDLAudio
( openAudio
, enableAudio
, putSampleSource
, lockAudio
, resumeAudio
, closeAudio
)
where

import Control.Concurrent.MVar

-- for SDL
import qualified SDL as SDL
import Data.Word (Word16) -- for buffer size
import qualified Data.Vector.Storable.Mutable as SMV
import Control.Monad (zipWithM_)

-- for SampleSource
import Sound.Hailstone.Synth (SampleSource, SampleRate, consumeSource)

-- | SDL audio callback function; called whenever SDL needs more data in the
-- audio buffer. This should copy samples into the buffer, or pad with silence
-- if there is no more data to play
sdlAudioCallback :: MVar SampleSource  -- MVar state of the sample source stream
                 -> SDL.AudioFormat s  -- SDL Audio Format identifier
                 -> SMV.IOVector s     -- Mutable IO vector containing samples
                 -> IO ()
sdlAudioCallback mSource sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  SDL.Signed16BitLEAudio -> do   -- turns out we need GADTs turned on for this
    sampleSrc <- takeMVar mSource
    -- consume audio from samplesource
    let buffLen = SMV.length sdlAudioBuffer
        (rawPCMbytes, restOfSrc) = consumeSource buffLen sampleSrc 
    -- then copy the bytes into the IO vector,
    zipWithM_ (SMV.write sdlAudioBuffer) [0..] rawPCMbytes

    -- update the sample source state MVar with the remainder of the source
    putMVar mSource restOfSrc
  _ -> putStrLn "Unsupported audio sample format"


-- | Initialize SDL (the Audio subsystem only) given a sample rate and buffer
-- size, and create an `MVar` to store the output `SampleSource` stream.
openAudio :: SampleRate -> Word16 -> IO (SDL.AudioDevice, MVar SampleSource)
openAudio sampleRate bufferSize = do
  SDL.initialize [SDL.InitAudio]
  let 
      sampleType = SDL.Signed16BitNativeAudio
  
  initialSource <- newMVar (pure 0)
  -- function "requests" an audio spec from the hardware, using the
  -- requests/demands we specify in the Spec. It returns a spec, that has the
  -- true values that were provided, which hopefully matches our demands but
  -- might not exactly match our demands. (the spec is the 2nd tuple item, but
  -- that is unused)
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate $ fromIntegral sampleRate
    , SDL.openDeviceFormat = SDL.Mandate sampleType
    , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
    , SDL.openDeviceSamples = bufferSize
    , SDL.openDeviceCallback = sdlAudioCallback initialSource
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing -- any output audio device will do
    }
  -- TODO we should grab (SDL.audioSpecSilence spec) and somehow tell the Synth
  -- module that that value is what represents silence (for now we default to 0)
  pure (device, initialSource)

-- | Closes SDL audio. Should be called at the end of main.
closeAudio :: SDL.AudioDevice -> IO ()
closeAudio = SDL.closeAudioDevice

-- | Modify the MVar to store a new sample source stream.
putSampleSource :: MVar SampleSource -> SampleSource -> IO ()
putSampleSource mv newSource = do
  _ <- takeMVar mv
  putMVar mv newSource

-- | Enable SDL audio. Should be called at the start of main.
enableAudio :: SDL.AudioDevice -> IO ()
enableAudio dev = SDL.setAudioDevicePlaybackState dev SDL.Play

-- | Pauses SDL audio, preventing the callback from firing I think.
lockAudio :: SDL.AudioDevice -> IO ()
lockAudio dev = SDL.setAudioDeviceLocked dev SDL.Locked

-- | Resumes SDL audio.
resumeAudio :: SDL.AudioDevice -> IO ()
resumeAudio dev = SDL.setAudioDeviceLocked dev SDL.Unlocked
