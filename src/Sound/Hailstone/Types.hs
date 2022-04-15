module Sound.Hailstone.Types
( -- * Type synonyms and datatypes
  -- ** Synth math value type
  SynthVal
  -- ** Synonyms for `SynthVal` for its different uses
, Freq, Vol, Pan, TimeVal
, unmaybePan
  -- ** Integer types
, SampleRate, SampleVal
  -- ** Note data & config
, ChanMode(..)
, Cell(..)
, ADSRParams(..)
) where

import Data.Int (Int16)

-- | Shared number type for all calculations in synthesis functions.
type SynthVal = Double

-- | Frequency value type in Hz; must be positive.
type Freq = SynthVal

-- | Volume/amplitude as a scale factor; normally between -1 and 1 when creating
-- signals that go to the speakers but for modulators this can just be anything.
type Vol = SynthVal

-- | Panning percentage as percentage Right; i.e. 0.0 is hard left, 0.5 is
-- centered, and 1.0 is hard right.
type Pan = SynthVal

-- | Constants denoting mono and stereo (easier to read than Bools for the job.)
data ChanMode = Mono | Stereo
  deriving (Show, Eq)

-- | Sample rate value in Hz (e.g. 44100 Hz, 48000 Hz)
type SampleRate = Int

-- | A time value type, which is used in both time tick values (see `timesteps`)
-- and durations (see `Cell`, `piecewise`, and related functions)
type TimeVal = SynthVal

-- | Type of a value to be sent to the audio backend. Here we use 16-bit
-- signed integer audio.
type SampleVal = Int16

-- | Note data cell; stores some basic playing properties of a note.
data Cell = Cell 
  { freqOf :: Freq    -- ^Frequency of the note
  , volOf :: Vol      -- ^Volume of the note (should be between 0.0 and 1.0)
  , durOf :: TimeVal  
  -- ^Duration of the note in seconds. This need not equal the total time
  -- setting in the ADSR parameters (see below)
  , panOf :: Maybe Pan 
  -- ^The panning value is a `Maybe Pan` so that a pan value of `Nothing` should
  -- result in the note being played as a true mono signal, while @`Just` p@
  -- would imply a note to be played at panning value @p@ in stereo.
  , adsrOf :: ADSRParams
  -- ^ The envelope settings for this note.
  }
  deriving (Show, Eq)

-- | Attack-decay-sustain-release envelope parameters. Values of type `SynthVal`
-- are between 0 and 1.
data ADSRParams = 
  ADSR { startLvlOf :: SynthVal
        -- ^The starting level for the envelope
       , attackTimeOf :: TimeVal
        -- ^Time taken to go from the starting level to 1.0
       , decayTimeOf :: TimeVal
        -- ^Time taken to decay from 1.0 to the sustain level
       , sustainLvlOf :: SynthVal
        -- ^Sustain level
       , releaseTimeOf :: TimeVal
        -- ^Time taken to release from sustain level to end level
       , endLvlOf :: SynthVal
        -- ^End level
       , totalTimeOf :: TimeVal
       -- ^Total time taken for the envelope (used to calculate sustain time)
       }
  deriving (Show, Eq)

-- | Unwraps Maybe Pan. Only useful if we're doing any sort of stereo at all;
-- defaults Nothing pan to mean 0.5
unmaybePan :: Maybe Pan -> Pan
unmaybePan = maybe 0.5 id
