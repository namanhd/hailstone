-- Test main executable, for trying out songs and synths

module Main where

import Sound.Hailstone.Synth
import Sound.Hailstone.SDLAudio
import Control.Concurrent (threadDelay)

testSong :: [Cell]
testSong = [ 
  Cell 440 0.25 0.5, Cell 550 0.25 0.5, Cell 660 0.25 1.0,
  Cell 660 0.0 1.0,
  Cell 660 0.25 0.5, Cell 550 0.25 0.5, Cell 440 0.25 1.0]

testSong2Chords :: [[Cell]]
testSong2Chords = let dur = 0.5; vol = 0.25 in
  [
    [Cell (0.5 * 440) vol dur, Cell (0.5 * 440 * 4 / 3) vol dur, Cell (0.5 * 440 * 8/9) vol dur],
    [Cell (440 * 5/6) vol dur, Cell (440 * (4/3) * (5/4)) vol dur, Cell (440 * (8/9) * (5/4)) vol dur]
  ]

-- | A test synth sound that works well as a SNES sample-like chord/keys sound
testSynth1 :: Stream Freq -> Stream Vol -> Stream TimeVal -> Stream Double
testSynth1 fs vs ts = finalSrc
  where
    -- convenience partially-applied function to prevent repeating "ts"
    sinSourceT = \pp ff vv -> sinSource pp ff vv ts

    -- First are the modulators that depend on the note being played:    

    -- Two submodulators,  each synced with (a function of) note frequencies
    sinModulatorSub1 = sinSourceT (pure 0.0) (fs) (pure 1.0)
    sinModulatorSub2 = sinSourceT (pure 0.0) ((2*)<$> fs) (pure 1.0)
    
    -- The submodulators modulate the phase of these modulators, whose
    -- frequency is also synced with a function of note frequencies
    sinModulator1 = sinSourceT sinModulatorSub1 ((2 *)<$> fs) (pure 1.0)
    sinModulator2 = sinSourceT sinModulatorSub2 ((3 *)<$> fs) (pure 0.8)
    
    -- Multiply the two modulators together
    sinModulator = (*) <$> sinModulator1 <*> sinModulator2

    -- function to mix in some vibrato. Note that the vibrato sin source's 
    -- volume is itself modulated by a slower sine, which creates a vibrato
    -- ramp-up effect as the note plays.
    applyVibrato = \ff -> (+) <$> ff <*> sinSourceT (pure 0.0) (pure 3.0) (sinSourceT (pure 0.0) (pure 2.0) (pure 0.5))
    -- this is the actual carrier of the (vibrato-applied) note frequency
    sinCarrier = sinSourceT sinModulator (applyVibrato fs) vs
    -- add some overtones for an "additive synth" color
    sinCarrierOvertone1 = sinSourceT sinModulator ((2*) <$> (applyVibrato fs)) ((0.3 *) <$> vs)
    sinCarrierOvertone2 = sinSourceT sinModulator ((3*) <$> (applyVibrato fs)) ((0.15 *) <$> vs)
    -- mix the base carrier and its overtones
    finalSrc = mix sinCarrier . mix sinCarrierOvertone1 $ sinCarrierOvertone2


-- | Low pass filter
applyLPfilter :: (Num a, Fractional a) => Int -> Stream a -> Stream a
applyLPfilter wndSize = constConvolve wndSize (windowWeights wndSize)
  where 
    windowWeights windowSize = fmap (* (1/fromIntegral windowSize)) 
      . take windowSize $ repeat 1


tonetestmainSDL :: IO ()
tonetestmainSDL = do
  let sampleRate = 44100
      bufferSize = 8192
  putStrLn "Opening audio"
  (audioDevice, mvSampleSrc) <- openAudio sampleRate bufferSize
  putStrLn "Making song sample stream"
  let ts = timesteps sampleRate
      -- summedSrc = applyLPfilter 3 $ retriggerWithNotes 0.0 0.0 ts testSynth1 testSong
      playNotes notes = applyLPfilter 3 $ retriggerWithNotes 0.0 0.0 ts testSynth1 notes
      summedSrc = foldl1 (\ta tb -> (+) <$> ta <*> tb) $ playNotes <$> testSong2Chords
      sampSrc = asPCM summedSrc
      
  putSampleSource mvSampleSrc sampSrc
  enableAudio audioDevice
  putStrLn "Starting play"
  let runDuration = 5.0 :: TimeVal
  threadDelay (round $ runDuration * 1000000)

  putStrLn "Done waiting"
  closeAudio audioDevice
  pure ()

main :: IO ()
main = tonetestmainSDL