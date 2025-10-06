{-# LANGUAGE OverloadedRecordDot, Strict #-}
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
  [ MkC 440 0.25 0.0
  , MkC 550 0.25 0.5
  , MkC 660 0.25 1.0
  , MkC 660 0.00 1.5
  , MkC 660 0.25 2.0
  , MkC 550 0.25 2.5
  , MkC 440 0.25 3.0
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
  [ MkC r 0.4 (b 0)
  , MkC (maj7 r) 0.25 (b 2), MkC (maj10 r) 0.25 (b 2)
  , MkC r 0.4 (b 3)
  , MkC (downp5 r) 0.4 (b 4)
  , MkC (maj3 r) 0.25 (b 5), MkC (maj7 r) 0.25 (b 5), MkC (maj10 r) 0.25 (b 5)
  , MkC (downp4 r) 0.4 (b 7)
  , MkC r 0.4 (b 8)
  , MkC (maj10 r) 0.25 (b 9), MkC (maj7 r) 0.25 (b 9)
  , MkC r 0.4 (b 11)
  , MkC (0.5 * r) 0.4 (b 12), MkC (maj10 r) 0.25 (b 12), MkC (p5 r) 0.25 (b 12)

  , MkC r 0.1 (b 14)
  , MkC (9/8 * r) 0.12 (b 14.5)
  , MkC (6/5 * r) 0.15 (b 15)
  , MkC (5/4 * r) 0.2 (b 15.5)
  , MkC (3/2 * r) 0.22 (b 16)
  , MkC (10/6 * r) 0.25 (b 16.5)
  , MkC (2 * r) 0.3 (b 17)
  ]

-- | shorthand constructor for `MkC`
n :: Freq -> Gain -> TimeVal -> TimeVal -> Maybe Pan -> ADSRParams -> Cell
n = MkC

testSynth0 :: Node LiveCell -> Node (LR SynthVal)
testSynth0 lc = finalNode
  where
    f = lc <&> (.freq)
    g = lc <&> (.gain)
    e = lc <&> (.env) -- note envelope current value
    -- pan = lc <&> (.pan)
    finalNode = m2s $ e * sinOsc g (f * (1 +| sinOsc (linearRamp 1.2 5 12) 0.02))

testSynth1 :: Node LiveCell -> Node (LR SynthVal)
testSynth1 lc = finalNode
  where
    f = cache $ lc <&> (.freq)
    g = lc <&> (.gain)
    e = cache $ lc <&> (.env) -- note envelope current value
    -- pan = lc <&> (.pan)
    sinModulator3 = adsrEnvelope (ADSR 0.0 0.02 0.0 1.0 0.02 0.01 1.0) * sinOsc e (5 *| f)
    fWithVibrato = (f * (1 +| sinOsc 0.02 (linearRamp 1.2 5 12)))
    sinCarrier = sinOscP g fWithVibrato sinModulator3
    finalNode = m2s $ e * sinCarrier

testSynth2 :: Node LiveCell -> Node (LR SynthVal)
testSynth2 lc = finalNode
  where
    f = cache $ lc <&> (.freq)
    g = cache $ lc <&> (.gain)
    e = cache $ lc <&> (.env) -- note envelope current value
    sinModulator1 = e * nADSR 0.1 0.04 0.0 1.0 0.05 0.01 1.0 * sinOsc 0.7 (5 *| f)
    -- also apply a pitch envelope on top of vibrato
    fWithVibrato = nADSR 0.0 0.01 0.0 1.0 0.05 0.01 1.0 * (f * (1 +| sinOsc 0.02 5))
    sinCarrier1 = sinOscP g fWithVibrato sinModulator1
    sinModulator2 = nADSR 1.0 0.0 0.0 1.0 0.0 0.07 0.0 * sinOsc 1.2 (6 *| f)
    sinCarrier2 = sinOscP g (2 *| fWithVibrato) sinModulator2
    finalNode = m2s $ e * (sinCarrier1 + sinCarrier2)


tonetestmainSDL :: IO ()
tonetestmainSDL = do
  let sampleRate = 44100
      bufferSize = 128
      chanMode = Stereo

  putStrLn "Making song sample stream"
  let
    sec = 1000000
    playNotes synth notes = retriggerWith
      EnvelopeIgnoresCellDuration RetrigPolyphonic 0.0 0.0 synth notes
    summedNode = playNotes testSynth2 testSong2
    destNode = asPCM summedNode

  putStrLn "Opening audio"
  hah <- openAudio sampleRate bufferSize chanMode destNode

  -- -- can replace node with
  -- putNode hah newDestNode

  putStrLn "Waiting before play start"
  threadDelay (round $ 0.1 * sec)
  putStrLn "Starting play"
  enableAudio hah

  let runDuration = 3.5 :: TimeVal
  threadDelay (round $ runDuration * sec)

  putStrLn "Done waiting"
  closeAudio hah
  pure ()

main :: IO ()
main = tonetestmainSDL