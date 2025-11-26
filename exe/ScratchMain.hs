{-# LANGUAGE OverloadedRecordDot, Strict, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- Test main executable, for trying out songs and synths

module Main where
import Sound.Hailstone.Synth.Node
import Sound.Hailstone.Synth.Generators
import Sound.Hailstone.Synth.Effects
import Sound.Hailstone.Sequencing
import qualified Sound.Hailstone.Sequencing.Cell as C
import Sound.Hailstone.Sequencing.CellScoreBuild

-- the backends are interchangeable
#ifdef HLSTN_AUDIO_BACKEND_SDL
import Sound.Hailstone.Backends.SDLAudio
#else
import Sound.Hailstone.Backends.PortAudio
#endif

import Control.Concurrent (threadDelay)

testSong :: [Cell e]
testSong = let
  nop = 0.5
  du = 0.5
  myAdsr = ADSR 0.01 0.0 0.1 0.2 0.9 0.9 0.0
  in (\cc -> cc du nop myAdsr) <$>
  [ MkC 440 0.25 0.0
  , MkC 550 0.25 0.5
  , MkC 660 0.25 1.0
  , MkC 660 0.00 1.5
  , MkC 660 0.25 2.0
  , MkC 550 0.25 2.5
  , MkC 440 0.25 3.0
  ]

testSong2 :: [Cell e]
testSong2 = let
  nop = 0.5
  sxthlen = 0.18
  b = (sxthlen *)
  myAdsr = ADSR 0.01 0.0 0.08 0.1 0.9 0.9 0.0
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

testSong3 :: ([Cell e], [Cell e])
testSong3 = let
  sxthlen = 0.19
  b = (sxthlen *)
  r x = x * 440
  rr x = (pure $ r x) :: Node e Freq
  a = 0.45
  aa = pure 0.38 :: Node e Ampl
  p = 0.5
  pp = pure p :: Node e Pan
  k t = ADSR 0.0 0.0 t (0.2 * t) 1.0 1.0 0
  kk t = adsr' (k t)
  topchords r1 r2 t =
    [MkC    (r $ r1)  (0.4 * a)     (b t)     blen (1.5 * p) kbh
    ,MkC    (r $ r2)  (0.3 * a)     (b t)     blen (1.5 * p) kbh
    ,MkC    (r $ r1)  (0.4 * a)     (b (t+2)) blen (1.5 * p) kbh
    ,MkC    (r $ r2)  (0.3 * a)     (b (t+2)) blen (1.5 * p) kbh
    ]
  glideup = (adsr 0.05 0.0 999.0 0.0 (8/9) 1.0 1.0 *)
  glidedown t = (adsr 0.0 0.0 t 0.05 1.0 1.0 (15.6/16) *)
  vib = (* (1 +| sinOsc (startAt (b 1.0) $ linearRamp (b 0.4) 0 0.015) 7))
  kbh = (k blen)
  pl = (p * 1.2)
  aba = (a * 0.6)
  blen = (b 0.5)
  in (
    [ MkAC  (vib $ glideup $ rr $ 2/3) (0.8 *| aa) (b 0) (b 4) (0.5 *| pp) (adsr (b 0.6) (b 0) (b 3.0) (b 0.1) 0.2 1.0 0)
    , MkAC  (vib $ glideup $ rr $ 3/4) (0.7 *| aa) (b 4) (b 2) (pp) (kk (b 2.0))
    , MkAC  (rr $ 5/6) (0.6 *| aa) (b 6) (b 2.0) (pp)  (kk (b 1.0))
    , MkAC  (glideup $ rr $ 15/16) (0.6 *| aa) (b 8) (b 2.0) (pp) (kk (b 1.5))
    , MkAC  (glidedown (b 1.0) $ glideup $ rr $ 75/64) (0.6 *| aa) (b 10) (b 2.0) (1.2 *| pp) (kk (b 1.5))
    , MkAC  (vib $ rr $ 1) (0.4 *| aa) (b 12) (b 4.0) (0.9 *| pp) (adsr (b 0.2) (b 0) (b 2.3) (b 1.0) 0.2 1.0 0)
    , MkAC  (vib $ glideup $ rr $ 4/3) (0.8 *| aa) (b 16) (b 2) (pp) (adsr (b 0.2) (b 0) (b 2.3) (b 1.0) 0.2 1.0 0)
    , MkAC  (rr $ 32/27) (0.5 *| aa) (b 18) (b 1) (pp) (kk (b 0.8))
    , MkAC  (rr $ 10/9)  (0.6 *| aa) (b 19) (b 1) (pp) (kk (b 0.4))
    , MkAC  (vib $ glideup $  rr $ 1.0) (0.6 *| aa) (b 20) (b 2) (pp) (adsr (b 0.2) (b 0) (b 2.3) (b 1.0) 0.2 1.0 0)
    , MkAC  (glideup $ rr $ 5/6) (0.6 *| aa) (b 22) (b 2) (pp) (kk (b 1.5))
    , MkAC  (vib $ glideup $ rr $ 15/16) (0.6 *| aa) (b 24) (b 3) (pp) (kk (b 2.3))
    , MkAC  (rr $ 3/4) (0.6 *| aa) (b 27) (b 1) (pp) (kk (b 0.5))
    , MkAC  (vib $ rr $ 5/6) (0.6 *| aa) (b 28) (b 3) (pp) (adsr (b 0.2) (b 0) (b 2.3) (b 1.0) 0.2 1.0 0)
    ]
  , [ MkC   (r $ 1/3) aba     (b 0) blen pl kbh
    , MkC   (r $ 1/4) aba     (b 2) blen pl kbh
    , MkC   (r $ 1/3) aba     (b 4) blen pl kbh
    , MkC   (r $ 1/4) aba     (b 6) blen pl kbh
    , MkC   (r $ 3/8) aba     (b 8) blen pl kbh
    , MkC   (r $ 9/32) aba    (b 10) blen pl kbh
    , MkC   (r $ 1/4) aba     (b 12) blen pl kbh
    , MkC   (r $ 1/3) aba     (b 14) blen pl kbh
    , MkC   (r $ 1/3) aba     (b 16) blen pl kbh
    , MkC   (r $ 1/4) aba     (b 18) blen pl kbh
    , MkC   (r $ 1/3) aba     (b 20) blen pl kbh
    , MkC   (r $ 1/4) aba     (b 22) blen pl kbh
    , MkC   (r $ 3/8) aba     (b 24) blen pl kbh
    , MkC   (r $ 9/32) aba    (b 26) blen pl kbh
    , MkC   (r $ 1/4) aba     (b 28) blen pl kbh
    , MkC   (r $ 1/3) aba     (b 30) blen pl kbh
    ]
    ++ concat [
      topchords 2 (5/3) 1, topchords 2 (5/3) 5, topchords (5/3) (30/16) 9, topchords (4/3) 2 13,
      topchords 2 (5/3) 17, topchords 2 (5/3) 21, topchords (5/3) (30/16) 25, topchords (4/3) 2 29
    ] )

testSong4csb :: CellScoreBuild e ()
testSong4csb = do
  set $ ampX 0.6
  add $ go itvl (3/2)
  add $ at itvl (5/6)
  add $ at itvl (2/3)
  add $ at itvl (5/8) <> andThen step 2

  lo <- add $ at itvl (1/4) <> at ampX 1.5 <> at susX 0.2 <> andThen step 3

  add $ at susX 0.2 <> andThen step 1

  b <- add $ go itvl (5/6)
  add $ go itvl (5/6)
  add $ go itvl (8/9)
  add $ go itvl (cents2ratio (-600)) <> at susX 0.2 <> andThen step 2
  visit lo $ andThen step 3

  c <- add $ go freq b.freq <> priv susX 0.2 <> andThen step 1
  rep 2 $ \i -> block $ do
    add $ go itvl (16/15)
    add $ go itvl (3/4)
    add $ go itvl (5/6)
    add $ go itvl (8/9) <> andThen step (if i == 0 then 3 else 2)

  visit lo $ andThen step 1
  visit c $ go itvl (8/9)
  add $ at itvl (5/6)
  add $ at itvl (5/6 * 8.1/9)
  add $ at itvl (5/6 * 8.1/9 * cents2ratio (-400)) <> andThen step 2
  visit lo $ andThen step 1

  b <- add $ go itvl (9/8)
  add $ go itvl (5/6)
  add $ go itvl (8/9)
  add $ go itvl (cents2ratio (-600)) <> at susX 0.2 <> andThen step 2

  add $ go freq b.freq <> go susX 0.4 <> go itvl (cents2ratio (-400))
  add $ at itvl (5/6)
  add $ at itvl (3/4)
  add $ go itvl (5/8) <> andThen step 3

  add $ go itvl (15/16) <> andThen step 1
  add $ go itvl (16/15) <> go susX 0.7 <> andThen step 2
  add $ go itvl (4/5) <> andThen step 4
  add $ go itvl (1/2)

  pure ()


testSong4 :: [([Cell e], Node e Now -> Node e (LR SynthVal))]
testSong4 = realizeCellScore
  "fmkeys"
  (MkC { C.freq=370, C.ampl=0.8, C.start=0, C.dur=0.135, C.pan=0.5, C.adsr=(ADSR 0.005 0 0.21 0.1 0.5 1.0 0.0)})
  [("fmkeys", testSynth4)]
  testSong4csb

testSynth0 :: Node e Now -> Node e (LR SynthVal)
testSynth0 now = finalNode
  where
    f = now.freq
    a = now.ampl
    e = now.env -- note envelope current value
    -- pan = now.pan
    finalNode = mono2stereo $ e * sinOsc a (f * (1 +| sinOsc 0.02 (linearRamp 1.2 5 12)))

testSynth1 :: Node e Now -> Node e (LR SynthVal)
testSynth1 now = finalNode
  where
    f = share $ now.freq
    a = now.ampl
    e = share $ now.env -- note envelope current value
    -- pan = now.pan
    sinModulator3 = adsr 0.015 0.0 0.02 0.01 0.5 1.0 1.0 * sinOsc e (5 *| f)
    fWithVibrato = (f * (1 +| sinOsc 0.02 (linearRamp 1.2 5 12)))
    sinCarrier = sinOscPM (0.8 *| a) fWithVibrato sinModulator3
    finalNode = mono2stereo $ e * sinCarrier

testSynth2 :: Node e Now -> Node e (LR SynthVal)
testSynth2 now = finalNode
  where
    f = share $ now.freq
    a = share $ now.ampl
    e = share $ now.env -- note envelope current value
    -- also apply a pitch envelope on top of vibrato
    fWithVibrato  = share $ adsr 0.01 0.0 0.05 0.01 0.0 1.0 1.0 * (f * (1 +| sinOsc 0.02 5))
    sinModulator1 = e * adsr 0.04 0.0 0.05 0.01 0.1 1.0 1.0 * triOsc 0.7 (5 *| f)
    sinCarrier1   = sinOscPM a fWithVibrato sinModulator1
    sinModulator2 = adsr 0.0 0.0 0.0 0.07 1.0 1.0 0.0 * triOsc 1.2 (7 *| f)
    sinCarrier2   = sinOscPM a (2 *| fWithVibrato) sinModulator2
    finalNode     = mono2stereo $ e * (sinCarrier1 + sinCarrier2)

testSynth3 :: Node e Now -> Node e (LR SynthVal)
testSynth3 now = finalNode
  where
    f = now.freq
    a = now.ampl
    e = now.env
    finalNode = lpf 1.0 6200 $ mono2stereo $ e * triOscPM a f (sqrOscDM 1.0 (7 *| f) (linearRamp 0.02 1 0))

testSynth4 :: Node e Now -> Node e (LR SynthVal)
testSynth4 now = finalNode
  where
    f = share $ now.freq
    a = now.ampl
    e = share $ now.env -- note envelope current value
    -- fWithVibrato = share $ (f * (nCents2ratio $ sinOsc 10 4))
    fWithVibrato = f
    sinModulator3 = adsr 0.015 0.0 0.01 0.01 0.5 1.0 0.5 * triOscPM (e) (3 *| fWithVibrato)
      (triOsc (0.2 * e) (fWithVibrato))

    sinCarrier = sinOscPM (0.8 *| a) fWithVibrato sinModulator3
    finalNode = lpf 1.0 6000 $  mono2stereo $ e * sinCarrier

tonetestmain :: IO ()
tonetestmain = do
  let sampleRate = 44100
      bufferSize = 64
      chanMode = Stereo

  putStrLn "Making song sample stream"
  let
    sec = 1000000
    playSong = retriggerWith
      EnvelopeIgnoresCellDuration RetrigPolyphonic 0.0 0.0
    -- mixed = playNotes testSynth1 testSong2
    -- (testSong3_part0, testSong3_part1) = testSong3
    -- mixed = playSong [(testSong3_part0, testSynth1), (testSong3_part1, testSynth3)]
    mixed = playSong testSong4
    master = echo' 96 0.5 0.4 1.0 800 0.2 mixed
    destNode = asPCM $ startAt 0.17 $ master

  putStrLn "Opening audio"
  withAudio sampleRate bufferSize chanMode () destNode $ \hah -> do
    -- -- can replace node with
    -- putNode hah () newDestNode
    putStrLn "Waiting before play start"
    putStrLn "Starting play"
    enableAudio hah

    let runDuration = 6.5 :: TimeVal
    threadDelay (round $ runDuration * sec)

    putStrLn "Done waiting"
    pure ()

main :: IO ()
main = tonetestmain