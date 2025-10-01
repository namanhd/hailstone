{-# LANGUAGE OverloadedRecordDot #-}
-- Test main executable, for trying out songs and synths

module Main where

import Data.Functor ((<&>))
import Sound.Hailstone.Synth
import Sound.Hailstone.Backends.SDLAudio
import Control.Concurrent (threadDelay)

testSong :: [Cell]
testSong = let
  nop = Nothing
  du = 0.5
  myAdsr = ADSR 0.9 0.01 0.0 0.9 0.1 0.2 0.0
  in (\cc -> cc du nop myAdsr) <$>
  [ Cell 440 0.25 0.0
  , Cell 550 0.25 0.5
  , Cell 660 0.25 1.0
  , Cell 660 0.00 1.5
  , Cell 660 0.25 2.0
  , Cell 550 0.25 2.5
  , Cell 440 0.25 3.0
  ]

testSong2 :: [Cell]
testSong2 = let
  nop = Nothing
  sxthlen = 0.18
  b = (sxthlen *)
  myAdsr = ADSR 0.9 0.01 0.0 0.9 0.08 0.1 0.0
  p5 = ((3/2) *)
  downp5 = ((2/3) *)
  downp4 = ((3/4) *)
  maj10 = ((10/4) *)
  maj7 = ((30/16) *)
  maj3 = ((5/4) *)
  r = 220
  in (\cc -> cc sxthlen nop myAdsr) <$>
  [ Cell r 0.4 (b 0)
  , Cell (maj7 r) 0.25 (b 2), Cell (maj10 r) 0.25 (b 2)
  , Cell r 0.4 (b 3)
  , Cell (downp5 r) 0.4 (b 4)
  , Cell (maj3 r) 0.25 (b 5), Cell (maj7 r) 0.25 (b 5), Cell (maj10 r) 0.25 (b 5)
  , Cell (downp4 r) 0.4 (b 7)
  , Cell r 0.4 (b 8)
  , Cell (maj10 r) 0.25 (b 9), Cell (maj7 r) 0.25 (b 9)
  , Cell r 0.4 (b 11)
  , Cell (0.5 * r) 0.4 (b 12), Cell (maj10 r) 0.25 (b 12), Cell (p5 r) 0.25 (b 12)

  , Cell r 0.1 (b 14)
  , Cell (9/8 * r) 0.12 (b 14.5)
  , Cell (6/5 * r) 0.15 (b 15)
  , Cell (5/4 * r) 0.2 (b 15.5)
  , Cell (3/2 * r) 0.22 (b 16)
  , Cell (10/6 * r) 0.25 (b 16.5)
  , Cell (2 * r) 0.3 (b 17)
  ]

-- | shorthand constructor for `Cell`
n :: Freq -> Gain -> TimeVal -> TimeVal -> Maybe Pan -> ADSRParams -> Cell
n = Cell

testSynth0 :: Node LiveCell -> Node (LR SynthVal)
testSynth0 lc = finalNode
  where
    f = lc <&> (.freq)
    g = lc <&> (.gain)
    e = lc <&> (.env) -- note envelope current value
    -- pan = lc <&> (.pan)
    finalNode = m2s $ e * sinOsc (f * (1 +| sinOsc (linearRamp 1.2 5 12) 0.02)) g

testSynth1 :: Node LiveCell -> Node (LR SynthVal)
testSynth1 lc = finalNode
  where
    f = cache $ lc <&> (.freq)
    g = lc <&> (.gain)
    e = cache $ lc <&> (.env) -- note envelope current value
    -- pan = lc <&> (.pan)
    sinModulator3 = adsrEnvelope (ADSR 0.0 0.02 0.0 1.0 0.02 0.01 1.0) * sinOsc (5 *| f) e
    fWithVibrato = (f * (1 +| sinOsc (linearRamp 1.2 5 12) 0.02))
    sinCarrier = sinOscP sinModulator3 fWithVibrato g
    finalNode = m2s $ e * sinCarrier

testSynth2 :: Node LiveCell -> Node (LR SynthVal)
testSynth2 lc = finalNode
  where
    f = cache $ lc <&> (.freq)
    g = cache $ lc <&> (.gain)
    e = cache $ lc <&> (.env) -- note envelope current value
    sinModulator1 = e * nADSR 0.1 0.04 0.0 1.0 0.05 0.01 1.0 * sinOsc (5 *| f) 0.7
    -- also apply a pitch envelope on top of vibrato
    fWithVibrato = nADSR 0.0 0.01 0.0 1.0 0.05 0.01 1.0 * (f * (1 +| sinOsc 5 0.02))
    sinCarrier1 = sinOscP sinModulator1 fWithVibrato g
    sinModulator2 = nADSR 1.0 0.0 0.0 1.0 0.0 0.07 0.0 * sinOsc (6 *| f) 1.2
    sinCarrier2 = sinOscP sinModulator2 (2 *| fWithVibrato) g
    finalNode = m2s $ e * (sinCarrier1 + sinCarrier2)


tonetestmainSDL :: IO ()
tonetestmainSDL = do
  let sampleRate = 44100
      bufferSize = 8192
      chanMode = Stereo
  putStrLn "Opening audio"
  (audioDevice, mSink) <- openAudio sampleRate bufferSize chanMode
  putStrLn "Making song sample stream"
  let
    playNotes synth notes = retriggerWith
      EnvelopeIgnoresCellDuration RetrigPolyphonic 0.0 0.0 synth notes
    summedNode = playNotes testSynth2 testSong2
    destNode = asPCM summedNode

  putNode mSink destNode
  enableAudio audioDevice
  putStrLn "Starting play"
  let runDuration = 3.5 :: TimeVal
  threadDelay (round $ runDuration * 1000000)

  putStrLn "Done waiting"
  closeAudio audioDevice
  pure ()

main :: IO ()
main = tonetestmainSDL