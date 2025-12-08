{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sound.Hailstone.Synth.MiscTypes
( -- * Miscellaneous types
  -- ** Strict tuples
  SPair(..)
, STriple(..)
, SQuadrup(..)
  -- ** Integer types
, SampleRate, SampleVal
  -- ** Mono/stereo
, ChanMode(..)
  -- ** ADSR
, ADSRParams(..)
, adsrTotalTime
) where

import Data.Int (Int16)
import Sound.Hailstone.Synth.SynthVal

-- | Channel mode.
data ChanMode = Mono | Stereo
  deriving (Show, Eq)

-- | Sample rate value in Hz (e.g. 44100 Hz, 48000 Hz). Needs to be Int because SDL expects
-- an integer sample rate
type SampleRate = Int

-- | Type of a value to be sent to the audio backend. Here we use 16-bit
-- signed integer audio.
type SampleVal = Int16

-- | Attack-decay-sustain-release envelope parameters. Values of type `SynthVal`
-- used here, which are "levels" or "gains", are between 0 and 1 inclusive.
data ADSRParams = ADSR
  { tA :: !TimeVal
    -- ^Time taken to go from the starting level to 1.0
  , tD :: !TimeVal
    -- ^Time taken to decay from 1.0 to the sustain level
  , tS :: !TimeVal
    -- ^Sustain time
  , tR :: !TimeVal
    -- ^Time taken to release from sustain level to end level
  , v0 :: !Percent
    -- ^The starting level for the envelope
  , v1 :: !Percent
    -- ^Sustain level
  , v2 :: !Percent
    -- ^End level
  } deriving (Show, Eq)

-- | Total time taken by the envelope, the sum of the A, D, S, R durations
adsrTotalTime :: ADSRParams -> TimeVal
adsrTotalTime (ADSR !tA !tD !tS !tR _ _ _)  = tA + tD + tS + tR
{-# INLINABLE adsrTotalTime #-}

-- | Strict pair.
data SPair a b = MkS2 !a !b
  deriving (Eq, Show)

-- | Strict triple.
data STriple a b c = MkS3 !a !b !c
  deriving (Eq, Show)

-- | Strict quadruple
data SQuadrup a b c d = MkS4 !a !b !c !d
  deriving (Eq, Show)
