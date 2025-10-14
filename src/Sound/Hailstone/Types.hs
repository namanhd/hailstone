{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, BangPatterns #-}

module Sound.Hailstone.Types
( -- * Type synonyms and datatypes
  -- ** Canonical math value type
  SynthVal
  -- ** Synonyms for `SynthVal` for its different uses
, Freq, Ampl, Gain, Pan, TimeVal
  -- ** Strict pair
, SP(..)
  -- ** Convenience LR type for tupling up two values, one for each stereo channel
, LR(..)
, dupLR
  -- ** Integer types
, SampleRate, SampleVal
  -- ** Note data & config
, ChanMode(..)
, Cell(..)
, LiveCell(..)
, ADSRParams(..)
, adsrTotalTime
) where

import Data.Int (Int16)

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

-- | Note data cell; stores some basic playing properties of a note.
data Cell = MkC
  { freq :: !Freq -- ^Frequency of the note
  , ampl :: !Ampl -- ^Amplitude of the note as a linear multiplier (should be between 0 and 1)
  , start :: !TimeVal -- ^Start time of the note in seconds.
  , dur :: !TimeVal
  -- ^Duration of the note in seconds, which needs not equal the `adsrTotalTime` of `_adsr`;
  -- this will, however, be the duration after which the cell is hard cut off.
  , pan :: !Pan
  -- ^Pan of a note, 0 is hard left, 0.5 is center, 1 is hard right.
  , adsr :: !ADSRParams
  -- ^ The envelope settings for this note.
  }
  deriving (Show, Eq)

-- | A "live cell" is what an instrument will receive from a `Node` to play a note specified
-- by a `Cell`, which describes high-level static information. The `Cell` might specify some
-- varying frequency or ampl (per-note portamento, vibrato, or volume slide effect) and this
-- is reflected as time-varying `LiveCell`s emitted to the instrument upon every new sample.
-- There may be more metadata here such as arbitrary modulation values that can be mapped to
-- any parameter in the instrument, like ModX & ModY on notes wrt. FL Studio native plugins.
data LiveCell = MkLC
  { freq :: !Freq -- ^ current frequency
  , ampl :: !Ampl -- ^ current amplitude
  , pan :: !Pan -- ^ current pan
  , env :: !SynthVal -- ^ current value of the envelope
  } deriving (Show, Eq)

-- TODO : generalized Cell that has puts all of these fields in nodes. That
-- way.... we can embed entire VOICES, and PATTERNS, into Cells. So we get a
-- playlist view/pattern sequencing mechanism, for free...

-- | Attack-decay-sustain-release envelope parameters. Values of type `SynthVal`
-- used here, which are "levels" or "gains", are between 0 and 1 inclusive.
data ADSRParams = ADSR
  { _tA :: !TimeVal
    -- ^Time taken to go from the starting level to 1.0
  , _tD :: !TimeVal
    -- ^Time taken to decay from 1.0 to the sustain level
  , _tS :: !TimeVal
    -- ^Sustain time
  , _tR :: !TimeVal
    -- ^Time taken to release from sustain level to end level
  , _v0 :: !SynthVal
    -- ^The starting level for the envelope
  , _v1 :: !SynthVal
    -- ^Sustain level
  , _v2 :: !SynthVal
    -- ^End level
  } deriving (Show, Eq)

-- | Total time taken by the envelope, the sum of the A, D, S, R durations
adsrTotalTime :: ADSRParams -> TimeVal
adsrTotalTime (ADSR !tA !tD !tS !tR _ _ _)  = tA + tD + tS + tR
{-# INLINABLE adsrTotalTime #-}

-- | Strict pair.
data SP a b = MkSP !a !b
  deriving (Eq, Show)

-- | Convenience tuple of two audio values, one for each channel (left and right).
data LR a = MkLR !a !a
  deriving (Eq, Show)

instance Functor LR where
  fmap f (MkLR al ar) = MkLR (f al) (f ar)

instance Applicative LR where
  pure = dupLR
  (MkLR fl fr) <*> (MkLR al ar) = MkLR (fl al) (fr ar)

-- | Trivially make a mono value into a stereo value.
dupLR :: a -> LR a
dupLR a = MkLR a a
{-# INLINABLE dupLR #-}

instance (Num a) => Num (LR a) where
  (MkLR al ar) + (MkLR bl br) = MkLR (al + bl) (ar + br)
  (MkLR al ar) * (MkLR bl br) = MkLR (al * bl) (ar * br)
  (MkLR al ar) - (MkLR bl br) = MkLR (al - bl) (ar - br)
  abs (MkLR al ar) = MkLR (abs al) (abs ar)
  negate (MkLR al ar) = MkLR (negate al) (negate ar)
  signum (MkLR al ar) = MkLR (signum al) (signum ar)
  fromInteger = dupLR . fromInteger

instance (Fractional a) => Fractional (LR a) where
  (MkLR al ar) / (MkLR bl br) = MkLR (al / bl) (ar / br)
  recip (MkLR al ar) = MkLR (recip al) (recip ar)
  fromRational = dupLR . fromRational
