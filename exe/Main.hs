{-# LANGUAGE GADTs #-}
-- Test main executable, for trying out songs and synths

module Main where

import Hailstone.Synth
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

-- for SDL
import qualified SDL as SDL
import qualified Data.Vector.Storable.Mutable as SMV
import Control.Monad (zipWithM_)

-- | SDL audio callback function; called whenever SDL needs more data in the
-- audio buffer. This should copy samples into the buffer, or pad with silence
-- if there is no more data to play
sdlAudioCallback :: MVar SampleSource  -- MVar state of the sample source stream
                 -> SDL.AudioFormat s  -- SDL Audio Format identifier
                 -> SMV.IOVector s     -- Mutable IO vector containing samples
                 -> IO ()
sdlAudioCallback mSource sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  SDL.Signed16BitLEAudio -> do   -- turns out we need GADTs turned on for this pattern match
    sampleSrc <- takeMVar mSource
    -- consume audio from samplesource
    let buffLen = SMV.length sdlAudioBuffer
        (rawPCMbytes, restOfSrc) = consumeSource buffLen sampleSrc 
    -- then copy the bytes into the IO vector,
    zipWithM_ (SMV.write sdlAudioBuffer) [0..] rawPCMbytes

    -- update the sample source state MVar with the remainder of the sample source
    putMVar mSource restOfSrc
  _ -> putStrLn "Unsupported audio sample format"


-- | Initialize SDL (Audio subsystem only) and creates an MVar to store the
-- output sample source stream
openAudio :: SampleRate -> IO (SDL.AudioDevice, MVar SampleSource)
openAudio sampleRate = do
  SDL.initialize [SDL.InitAudio]
  let 
    -- emptyNote = Cell { freqOf = 440, volOf = 0.0 }
      bufferSize = 4096*2
      sampleType = SDL.Signed16BitNativeAudio
  
  initialSource <- newMVar (pure 0)
  -- function "requests" an audio spec from the hardware, using the
  -- requests/demands we specify in the Spec. It returns a spec, that has the
  -- true values that were provided, which hopefully matches our demands but
  -- might not exactly match our demands.
  (device, spec) <- SDL.openAudioDevice SDL.OpenDeviceSpec
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

-- | "Pauses" SDL audio, preventing the callback from firing I think.
lockAudio :: SDL.AudioDevice -> IO ()
lockAudio dev = SDL.setAudioDeviceLocked dev SDL.Locked

-- | "Resumes" SDL audio.
resumeAudio :: SDL.AudioDevice -> IO ()
resumeAudio dev = SDL.setAudioDeviceLocked dev SDL.Unlocked


testSong :: [Cell]
testSong = [ 
  Cell 440 0.25 0.5, Cell 550 0.25 0.5, Cell 660 0.25 1.0,
  Cell 660 0.0 1.0,
  Cell 660 0.25 0.5, Cell 550 0.25 0.5, Cell 440 0.25 1.0]

testSong2Chords :: [[Cell]]
testSong2Chords = 
  [
    [Cell 440 0.25 1.25, Cell (440 * 4 / 3) 0.25 1.25, Cell (440 * 8/9) 0.25 1.25],
    [Cell (440 * 5/6) 0.25 1.25, Cell (440 * (4/3) * (5/4)) 0.25 1.25, Cell (440 * (8/9) * (5/4)) 0.25 1.25]
  ]

testSynth1 :: Stream Freq -> Stream Vol -> Stream TimeVal -> SampleSource
testSynth1 songFreqs songVols ts = sampSrc
  where
    sinModulatorSub1 = sinSource (pure 0.0) (songFreqs) (pure 1.0) ts
    sinModulatorSub2 = sinSource (pure 0.0) ((2*)<$> songFreqs) (pure 1.0) ts
    sinModulator1 = sinSource sinModulatorSub1 ((2 *)<$> songFreqs) (pure 1.0) ts
    sinModulator2 = sinSource sinModulatorSub2 ((3 *)<$>songFreqs) (pure 0.8) ts
    sinModulator = (*) <$> sinModulator1 <*> sinModulator2
    shittyVibratoFreqs = (+) <$> songFreqs <*> sinSource (pure 0.0) (pure 4.0) (sinSource (pure 0.0) (pure 2.0) (pure 0.1) ts) ts
    sinCarrier = sinSource sinModulator shittyVibratoFreqs songVols ts
    sinCarrierOvertone1 = sinSource sinModulator ((2*) <$> songFreqs) ((0.3 *) <$> songVols) ts
    sinCarrierOvertone2 = sinSource sinModulator ((3*) <$> songFreqs) ((0.15 *) <$> songVols) ts
    summedSrc = mix sinCarrier . mix sinCarrierOvertone1 $ sinCarrierOvertone2
    sampSrc = asPCM summedSrc


tonetestmainSDL :: IO ()
tonetestmainSDL = do
  let sampleRate = 44100
  (audioDevice, mvSampleSrc) <- openAudio sampleRate
  
  let ts = timesteps sampleRate
      -- sinSource phaseSrc freqSrc volSrc timeSrc
      (songFreqs, songVols) = notesToSource 0 ts testSong
      sampSrc = testSynth1 songFreqs songVols ts

  putSampleSource mvSampleSrc sampSrc
  enableAudio audioDevice
  threadDelay (20 * 1000000)


  closeAudio audioDevice
  pure ()

main :: IO ()
main = tonetestmainSDL