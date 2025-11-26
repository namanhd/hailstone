{-# LANGUAGE Strict #-}
module Sound.Hailstone.Synth.SynthVal where

-- | Shared number type for all calculations in synthesis functions.
type SynthVal = Double

-- | Frequency value type in Hz; must be positive.
type Freq = SynthVal

-- | Interval type, given as cents
type ItvlCents = SynthVal

-- | Interval type, given as a (floating point) scale factor or ratio
type ItvlRatio = SynthVal

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

-- | A time value type, which is used in both time tick values and durations
type TimeVal = SynthVal

-- | @NaN@ literal
nan :: SynthVal
nan = 0 / 0

-- | @Infinity@ literal
inf :: SynthVal
inf = 1 / 0

-- | the Prelude @isNaN@ goes through some unsafe IO primop, this is probably faster
isNaN :: SynthVal -> Bool
isNaN a = a /= a

-- | two pi
twopi :: SynthVal
twopi = 2 * pi