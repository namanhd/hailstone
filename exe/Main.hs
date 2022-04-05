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
testSong2Chords = let dur = 0.8; vol = 0.2; panL = Just 0.2; panR = Just 0.8; panC = Just 0.5 in
  [
    [Cell (0.5 * 440) vol dur panL, Cell (0.5 * 440 * 4 / 3) vol dur panL, Cell (0.5 * 440 * 8/9) vol dur panL],
    [Cell (440 * 6/5) vol dur panR, Cell (220 * (4/3) * (5/4)) vol dur (Just 0.1), Cell (440 * (8/9) * (5/4)) vol dur panR],
    [Cell (440 * 3/2) vol dur panC, Cell (440 * (3/2) * (9/8)) vol dur panC, Cell (880 * (8/9) ) vol dur panC ]
  ]

n = Cell -- shorthand constructor

testSong3 :: [[Cell]]
testSong3 = let dur = 0.6; vol = 0.2; panL = Just 0.2; panR = Just 0.8; panC = Just 0.5; oct=2.0
                min3 = 6/5; maj3 = 5/4; p4 = 4/3; p5 = 3/2; maj2 = 9/8; min2 = 16/15; al=330.0 in
  [
    (\(Cell f v d p) -> Cell (f/2) v d p) <$> [n al vol dur panC, n (al/min3) vol dur panC, n (al*maj2) vol dur panC, n (al*maj2/oct) vol dur panC, 
     n (al/p4) vol dur panC, n (al*maj2/oct) vol dur panC, n (al/p4/oct) vol dur panC]
  ,  [n al vol dur panC, n (al/min3) vol dur panC, n (al*maj2) vol dur panC, n (al*maj2/oct) vol dur panC, 
     n (al/p4) vol dur panC, n (al*maj2/oct) vol dur panC, n (al/p4/oct) vol dur panC]
  , [n (al*maj3) vol dur panL, n (al) vol dur panL, n (al*maj2*maj3) vol dur panL, n (al*maj2/oct*(7/4)) vol dur panL, 
     n (al/p4*maj3) vol dur panL, n (al*maj2/oct*p5) (vol*0.5) dur panL, n (al/p4/oct*maj3) (vol*0.3) dur panL]
  , [n (al*maj3*min3) vol dur panR, n (al*maj3) vol dur panR, n (al*maj2*maj3*min3) vol dur panR, n (al*maj2/oct*(7/4)*min3) vol dur panR, 
     n (al/p4*maj3*min3) vol dur panR, n (al*maj2/oct*p5/min3) (vol*0.5) dur panR, n (al/p4/oct*maj3*min3) (vol*0.3) dur panR]
  , [n (al*maj3*min3*maj3) vol dur panC, n (al*maj3*min3) vol dur panC, n (al*maj2*maj3*min3*p4) vol dur panC, n (al*maj2*maj3*min3*p4/maj2) vol dur panC, 
     n (al*maj2*maj3*min3*p4/maj2/min2) vol dur panC, n (al*maj2/oct*p5/min3) (vol*0.2) dur panC, n (al/p4/oct*maj3*min3) (vol*0.3) dur panC]
  ]
-- | test synth sound
testSynth1 :: Stream Freq -> Stream Vol -> Stream TimeVal -> Stream Double
testSynth1 fs vs ts = finalSrc
  where
    -- convenience partially-applied function to prevent repeating "ts"
    sinSourceT = \pp ff vv -> sinSource pp ff vv ts
    -- First are the modulators that depend on the note being played:    
    sinModulatorSub1 = sinSourceT (pure 0.0) (fs) (pure 1.0)
    
    -- The submodulator modulates the phase of these modulator(s), whose
    -- frequency is also synced with a function of note frequencies
    sinModulator1 = sinSourceT sinModulatorSub1 (2 *| fs) (pure 1.0)
    sinModulator = sinModulator1
    -- function to mix in some vibrato. Note that the vibrato sin source's 
    -- volume is itself modulated by a slower sine, which creates a vibrato
    -- ramp-up effect as the note plays.
    applyVibrato = \ff -> ff |+| sinSourceT (pure 0.0) (pure 2.0) (sinSourceT (pure 0.0) (pure 1.0) (pure 0.3))
    -- this is the actual carrier of the (vibrato-applied) note frequency
    sinCarrier = sinSourceT sinModulator (applyVibrato fs) vs
    -- add some overtones for an "additive synth" color
    sinCarrierOvertone1 = sinSourceT sinModulator (2 *| applyVibrato fs) (0.3 *| vs)
    finalSrc = sinCarrier |+| sinCarrierOvertone1


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
      chanMode = Stereo
  putStrLn "Opening audio"
  (audioDevice, mvSampleSrc) <- openAudio sampleRate bufferSize chanMode
  putStrLn "Making song sample stream"
  let ts = timesteps sampleRate
      -- summedSrc = applyLPfilter 3 $ retriggerWithNotes 0.0 0.0 ts testSynth1 testSong
      playNotes notes = -- applyLPfilter 4 $ 
        retriggerWithNotes chanMode 0.0 0.0 ts testSynth1 notes
      summedSrc = mixAll $ playNotes <$> testSong3
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