{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sound.Hailstone.Types
( -- * Type synonyms and datatypes
  -- ** Canonical math value type
  SynthVal
  -- ** Synonyms for `SynthVal` for its different uses
, Freq, Ampl, Gain, Pan, Phase, Percent, TimeVal
  -- ** Strict tuples
, SPair(..)
, STriple(..)
  -- *** Strict stereo pair
, module Sound.Hailstone.Types.LR
  -- ** Integer types
, SampleRate, SampleVal
  -- ** Note data & config
, ChanMode(..)
, ADSRParams(..)
, adsrTotalTime
  -- ** For RBJ audio EQ cookbook 2nd-order filters
, CookbookFilterCoeffs(..)
, CookbookFilterState(..)
) where

import Data.Int (Int16)
import Sound.Hailstone.Types.LR

-- | Shared number type for all calculations in synthesis functions.
type SynthVal = Double

-- | Frequency value type in Hz; must be positive.
type Freq = SynthVal

-- | Interval type, given as cents
type Cents = SynthVal

-- | Amplitude as a scale factor; normally between -1 and 1 when creating
-- signals that go to the speakers but for modulators this can just be anything.
type Ampl = SynthVal

-- | Gain in decibels, where a multiplier or amplitude is @10^(dBgain / 20)@
type Gain = SynthVal

-- | Panning percentage as percentage Right; i.e. 0.0 is hard left, 0.5 is
-- centered, and 1.0 is hard right.
type Pan = SynthVal

-- | Generic percentage value, 0.0 to 1.0
type Percent = SynthVal

-- | Angle or phase in radians
type Phase = SynthVal

-- | Channel mode.
data ChanMode = Mono | Stereo
  deriving (Show, Eq)

-- | Sample rate value in Hz (e.g. 44100 Hz, 48000 Hz). Needs to be Int because SDL expects
-- an integer sample rate
type SampleRate = Int

-- | A time value type, which is used in both time tick values (see `timesteps`)
-- and durations (see `Cell`, `piecewise`, and related functions)
type TimeVal = SynthVal

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

-- | see <https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html>
data CookbookFilterCoeffs = MkCFCoeffs
  { a0inv :: !SynthVal
  , a1 :: !SynthVal
  , a2 :: !SynthVal
  , b0 :: !SynthVal
  , b1 :: !SynthVal
  , b2 :: !SynthVal
  } deriving (Show, Eq)

-- | see <https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html>
data CookbookFilterState = MkCFState
  { xm0 :: !(LR SynthVal)
  , xm1 :: !(LR SynthVal)
  , xm2 :: !(LR SynthVal)
  , ym0 :: !(LR SynthVal)
  , ym1 :: !(LR SynthVal)
  , ym2 :: !(LR SynthVal)
  }