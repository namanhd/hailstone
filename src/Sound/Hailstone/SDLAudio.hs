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

import Sound.Hailstone.SynthR

data SignalState = 
  SigState { getSampleSource :: SampleSource
           , getCurrTime :: TimeVal
           , getDeltaTime :: TimeVal
           , getCurrChan :: CurrChan
           , getChanMode :: ChanMode
           }

initSignalState :: SampleRate -> ChanMode -> SignalState
initSignalState sampleRate chanMode = 
  SigState { getSampleSource = pure 0
           , getCurrTime = 0.0
           , getDeltaTime = (1 / fromIntegral sampleRate)
           , getCurrChan = if chanMode == Mono then CMono else CLeft
           , getChanMode = chanMode
           }

consumeSignalState :: Int -> SignalState -> SMV.IOVector SampleVal -> IO SignalState
consumeSignalState nSteps sigState audioBuf = 
  mapM_ writer ([0..nSteps - 1]) *> pure modifiedState
  where
    chanLookahead :: ChanMode -> CurrChan -> Int -> CurrChan
    chanLookahead cm' c' i' = if cm' == Mono then CMono else case c' of
      CMono -> CMono
      CLeft -> if (i' `rem` 2) == 0 then CLeft else CRight
      CRight -> if (i' `rem` 2) == 0 then CRight else CLeft
    
    timeLookahead :: ChanMode -> TimeVal -> Int -> TimeVal -> TimeVal
    timeLookahead cm' t' n' d' = if cm' == Mono then t' + (fromIntegral n') * d'
      else t' + (fromIntegral $ n' `div` 2) * d'

    d = getDeltaTime sigState       -- delta time (= 1/sample rate)
    sig = getSampleSource sigState  -- signal
    t0 = getCurrTime sigState       -- start time for this chunk
    c0 = getCurrChan sigState       -- start channel for this chunk
    cm = getChanMode sigState       -- channel mode of signal

    modifiedState = sigState 
     { getCurrTime = timeLookahead cm t0 nSteps d
     , getCurrChan = chanLookahead cm c0 nSteps
     }
     
    writer :: Int -> IO ()
    writer idx = SMV.write audioBuf idx sampleVal
      where
        t = timeLookahead cm t0 idx d
        c = chanLookahead cm c0 idx
        sampleVal = sampleSignalAt t d c sig  -- sample the signal!!


-- | SDL audio callback function; called whenever SDL needs more data in the
-- audio buffer. This should copy samples into the buffer, or pad with silence
-- if there is no more data to play
sdlAudioCallback :: MVar SignalState  -- MVar state of the sample source stream
                 -> SDL.AudioFormat s  -- SDL Audio Format identifier
                 -> SMV.IOVector s     -- Mutable IO vector containing samples
                 -> IO ()
sdlAudioCallback mSigState sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  SDL.Signed16BitLEAudio -> do   -- turns out we need GADTs turned on for this
    signalState <- takeMVar mSigState
    let buffLen = SMV.length sdlAudioBuffer
    -- then copy the bytes into the IO vector, and consume the state throughout
    modifiedSigState <- consumeSignalState buffLen signalState sdlAudioBuffer

    -- update the signal state MVar with the remainder of the signal
    putMVar mSigState modifiedSigState
  _ -> putStrLn "Unsupported audio sample format"


-- | Initialize SDL (the Audio subsystem only) given a sample rate and buffer
-- size, and create an `MVar` to store the output `SampleSource` signal.
openAudio :: SampleRate -> Word16 -> ChanMode -> IO (SDL.AudioDevice, MVar SignalState)
openAudio sampleRate bufferSize chanMode = do
  SDL.initialize [SDL.InitAudio]
  let sampleType = SDL.Signed16BitNativeAudio
      stereoMode = case chanMode of
        Mono -> SDL.Mono
        Stereo -> SDL.Stereo
  
  initialSource <- newMVar $ initSignalState sampleRate chanMode
  -- function "requests" an audio spec from the hardware, using the
  -- requests/demands we specify in the Spec. It returns a spec, that has the
  -- true values that were provided, which hopefully matches our demands but
  -- might not exactly match our demands. (the spec is the 2nd tuple item, but
  -- that is unused)
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate $ fromIntegral sampleRate
    , SDL.openDeviceFormat = SDL.Mandate sampleType
    , SDL.openDeviceChannels = SDL.Mandate stereoMode
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
putSampleSource :: MVar SignalState -> SampleSource -> IO ()
putSampleSource mv newSource = do
  firstSigState <- takeMVar mv
  -- update the sample source and reset the time counter, but don't change
  -- anything about the delta value and the curr-chan
  let newSigState = firstSigState { getSampleSource = newSource, getCurrTime = 0.0 }
  putMVar mv newSigState

-- | Enable SDL audio. Should be called at the start of main.
enableAudio :: SDL.AudioDevice -> IO ()
enableAudio dev = SDL.setAudioDevicePlaybackState dev SDL.Play

-- | Pauses SDL audio, preventing the callback from firing I think.
lockAudio :: SDL.AudioDevice -> IO ()
lockAudio dev = SDL.setAudioDeviceLocked dev SDL.Locked

-- | Resumes SDL audio.
resumeAudio :: SDL.AudioDevice -> IO ()
resumeAudio dev = SDL.setAudioDeviceLocked dev SDL.Unlocked
