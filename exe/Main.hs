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
testSong3 = let dur = 2.0; vol = 0.2; panL = Just 0.2; panR = Just 0.8; panC = Just 0.5; oct=2.0
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

testSynth1 :: Signal Freq -> Signal Vol -> Signal SynthVal
testSynth1 fs vs = finalSrc
  where
    sinModulatorSub1 = sinSource (pure 0.0) fs (pure 1.0)
    sinModulator = sinSource sinModulatorSub1 (2 *| fs) (pure 1.0)
    fsWithVibrato = fs |+| sinSource (pure 0.0) (pure 1.1) (sinSource (pure (-0.1)) (pure 0.1) (pure 0.5))
    sinCarrier = sinSource sinModulator fsWithVibrato vs
    sinCarrierOvertone = sinSource sinModulator (2 *| fs) (0.3 *| vs)
    finalSrc = sinCarrier |+| sinCarrierOvertone



tonetestmainSDL :: IO ()
tonetestmainSDL = do
  let sampleRate = 44100
      bufferSize = 4096
      chanMode = Stereo
  putStrLn "Opening audio"
  (audioDevice, mSigState) <- openAudio sampleRate bufferSize chanMode
  putStrLn "Making song sample stream"
  let playNotes synth notes = retriggerWithNotes 0.0 synth notes
      -- filtering with raw convolutions is PAINFULLY slow, I should probably
      -- come up with an FFT at some point. 
      summedSrc = mixAll $ playNotes testSynth1 <$> testSong3
      -- add some delay!! here, set to 0.15 seconds of delay
      delaySrc = constStereoize 0.2 $ delay 0.0 0.15 summedSrc
      summedAndDelay = 0.7 *| summedSrc |+| 0.4 *| delaySrc
      sampSrc = asPCM summedAndDelay
      
  putSampleSource mSigState sampSrc
  enableAudio audioDevice
  putStrLn "Starting play"
  let runDuration = 16.0 :: TimeVal
  threadDelay (round $ runDuration * 1000000)

  putStrLn "Done waiting"
  closeAudio audioDevice
  pure ()

main :: IO ()
main = tonetestmainSDL