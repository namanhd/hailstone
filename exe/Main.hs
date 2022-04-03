-- Test main executable, for trying out songs and synths

module Main where

import Sound.Hailstone.Synth
import Sound.Hailstone.SDLAudio
import Control.Concurrent (threadDelay)

testSong :: [Cell]
testSong = let nop = Nothing in [ 
  Cell 440 0.25 0.5 nop, Cell 550 0.25 0.5 nop, Cell 660 0.25 1.0 nop,
  Cell 660 0.0 1.0 nop,
  Cell 660 0.25 0.5 nop, Cell 550 0.25 0.5 nop, Cell 440 0.25 1.0 nop]

testSong2Chords :: [[Cell]]
testSong2Chords = let dur = 1.0; vol = 0.2; panL = Just 0.2; panR = Just 0.8; panC = Just 0.5 in
  [
    [Cell (0.5 * 440) vol dur panL, Cell (0.5 * 440 * 4 / 3) vol dur panL, Cell (0.5 * 440 * 8/9) vol dur panL],
    [Cell (440 * 6/5) vol dur panR, Cell (220 * (4/3) * (5/4)) vol dur (Just 0.1), Cell (440 * (8/9) * (5/4)) vol dur panR],
    [Cell (440 * 3/2) vol dur panC, Cell (440 * (3/2) * (9/8)) vol dur panC, Cell (880 * (8/9) ) vol dur panC ]
  ]

-- | A test synth sound that works well as a SNES sample-like chord/keys sound
testSynth1 :: Stream Freq -> Stream Vol -> Stream TimeVal -> Stream Double
testSynth1 fs vs ts = finalSrc
  where
    -- convenience partially-applied function to prevent repeating "ts"
    sinSourceT = \pp ff vv -> sinSource pp ff vv ts

    -- First are the modulators that depend on the note being played:    

    -- submodulator(s),  each synced with (a function of) note frequencies
    sinModulatorSub1 = sinSourceT (pure 0.0) (fs) (pure 1.0)
    -- sinModulatorSub2 = sinSourceT (pure 0.0) (2 *| fs) (pure 1.0)
    
    -- The submodulator(s) modulates the phase of these modulator(s), whose
    -- frequency is also synced with a function of note frequencies
    sinModulator1 = sinSourceT sinModulatorSub1 (2 *| fs) (pure 1.0)
    -- sinModulator2 = sinSourceT sinModulatorSub2 (3 *| fs) (pure 0.8)
    
    -- Multiply the two modulators together
    sinModulator = sinModulator1 -- |*| sinModulator2

    -- function to mix in some vibrato. Note that the vibrato sin source's 
    -- volume is itself modulated by a slower sine, which creates a vibrato
    -- ramp-up effect as the note plays.
    applyVibrato = \ff -> ff |+| sinSourceT (pure 0.0) (pure 2.0) (sinSourceT (pure 0.0) (pure 1.0) (pure 0.3))
    -- this is the actual carrier of the (vibrato-applied) note frequency
    sinCarrier = sinSourceT sinModulator (applyVibrato fs) vs
    -- add some overtones for an "additive synth" color
    sinCarrierOvertone1 = sinSourceT sinModulator (2 *| applyVibrato fs) (0.3 *| vs)
    -- sinCarrierOvertone2 = sinSourceT sinModulator (3 *| applyVibrato fs) (0.15 *| vs)
    -- mix the base carrier and its overtones
    finalSrc = sinCarrier |+| sinCarrierOvertone1 -- |+| sinCarrierOvertone2


-- | Low pass filter
applyLPfilter :: (Num a, Fractional a) => Int -> Stream a -> Stream a
applyLPfilter wndSize = constConvolve wndSize (windowWeights wndSize)
  where 
    windowWeights windowSize = fmap (* (1/fromIntegral windowSize)) 
      . take windowSize $ repeat 1


tonetestmainSDL :: IO ()
tonetestmainSDL = do
  let sampleRate = 40000
      bufferSize = 8192
      chanMode = Stereo
  putStrLn "Opening audio"
  (audioDevice, mvSampleSrc) <- openAudio sampleRate bufferSize chanMode
  putStrLn "Making song sample stream"
  let ts = timesteps sampleRate
      -- summedSrc = applyLPfilter 3 $ retriggerWithNotes 0.0 0.0 ts testSynth1 testSong
      playNotes notes = -- applyLPfilter 4 $ 
        retriggerWithNotes chanMode 0.0 0.0 ts testSynth1 notes
      summedSrc = mixAll $ playNotes <$> testSong2Chords
      -- add some delay!! here, set to 0.15 seconds of delay
      delaySrc = piecewise chanMode 0 (0.15) [(summedSrc, 10.0)] ts
      summedAndDelay = 0.75 *| summedSrc |+| 0.25 *| delaySrc
      sampSrc = asPCM summedAndDelay
      
  putSampleSource mvSampleSrc sampSrc
  enableAudio audioDevice
  putStrLn "Starting play"
  let runDuration = 6.0 :: TimeVal
  threadDelay (round $ runDuration * 1000000)

  putStrLn "Done waiting"
  closeAudio audioDevice
  pure ()

main :: IO ()
main = tonetestmainSDL