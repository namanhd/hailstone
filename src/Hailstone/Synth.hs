module Hailstone.Synth 
( Freq, Vol, SampleRate, TimeVal, SampleVal
, Cell(..)
, Stream
, SampleSource
, consumeSource
, asPCM

, timesteps, clip, mix, zeros
, waveformSource, sinSource, squareSource
, piecewise
, notesToSource
)
where

import Data.Int (Int16)
import qualified Data.Stream as STR

type Freq = Double
type Vol = Double
type SampleRate = Int
type TimeVal = Double
type SampleVal = Int16

-- | Note data cell; stores some basic playing properties of a note
data Cell = Cell { freqOf :: Freq, volOf :: Vol, durOf :: TimeVal }
  deriving (Show, Eq)

type Stream = STR.Stream -- shorthand for convenience
type SampleSource = Stream SampleVal

-- | A stream of "time step values" t (for waveform-generating functions), 
-- sampled depending on the sample rate
timesteps :: SampleRate -> Stream TimeVal
timesteps sampleRate = ((1/fromIntegral sampleRate) *) <$> STR.fromInfList [0 :: TimeVal ..]

-- | Clip a sample source to the bounds allowed by the sample format.
clip :: SampleSource -> SampleSource
clip = fmap (min (maxBound :: SampleVal)) . fmap (max (minBound :: SampleVal))

-- | Scale a stream by a multiplier.
scale :: (Num a) => a -> Stream a -> Stream a
scale multiplier = fmap (multiplier *)

-- | Mix two sample sources (actually defined to be a bit more general
-- to work on any stream of values that can do addition).
mix :: (Num a) => Stream a -> Stream a -> Stream a
mix s1 s2 = STR.zipWith (+) s1 s2

-- | All-zeroes stream of samples.
zeros :: SampleSource
zeros = pure 0

-- | Convert a double-generating source (such as a raw waveform stream) into a
-- stream of audio sample values in our audio setup (here, Int16), with scaling
-- up to sample values followed by rounding and clipping
asPCM :: Stream Double -> SampleSource
asPCM = clip . fmap round . scale (fromIntegral (maxBound :: SampleVal))

-- | Generic waveform source (a waveform function that can be used is any
-- (Double -> Double) function with domain [0, inf) and range [-1, 1])
waveformSource :: (Double -> Double) -> Stream Freq -> Stream Vol -> Stream TimeVal -> Stream Double
waveformSource waveFn fs vs ts = (\f v t -> v * waveFn (f * t)) <$> fs <*> vs <*> ts

-- | Sinusoidal waveform source; supports modulation of phase, frequency,
-- volume, and time scale. TODO use a fastSin instead of this maybe
sinSource :: Stream Double -> Stream Freq -> Stream Vol -> Stream TimeVal -> Stream Double
sinSource phases fs vs ts = 
  (\ph f v t -> v * sin (2 * pi * f * t + ph)) <$> phases <*> fs <*> vs <*> ts

-- | Square waveform source; supports modulation of frequency, volume, and time
-- scale. (Duty cycle might become a parameter later down the line)
squareSource :: Stream Freq -> Stream Vol -> Stream TimeVal -> Stream Double
squareSource fs vs ts = (\f v t -> let ft = f * t in v * fromIntegral 
  ((2 * (2 * floor ft - floor (2 * ft)) + 1) :: Int)) <$> fs <*> vs <*> ts


-- | Funny fold/mapAccum-ish helper for `piecewise`, but may be useful
-- elsewhere. Carries out a foldl with a base accumulator value and an
-- accumulator function but also takes a "decorator function" as argument to
-- combine the current list item with the current accumulator value and obtain
-- an item in the output list (the type @b@ in the function's type.) Common use
-- case is to "decorate" the list with the accumulator value alongside folding.
-- Outputs both the accumulator result and the resulting decorated list.
-- Example usage:
-- > ghci> cascadeMap 0 (\acc (c, n) -> acc + n) (\acc (c, n) -> (c, n, acc)) 
-- >       $ [('a',1),('b',5),('c',3)]
-- > ([('a',1,0),('b',5,1),('c',3,6)],9)
cascadeMap :: x -> (x -> a -> x) -> (x -> a -> b) -> [a] -> ([b], x)
cascadeMap baseAccumVal _ _ [] = ([], baseAccumVal)
cascadeMap baseAccumVal accumFn decorateFn (a:as) = 
  let newAccumVal = accumFn baseAccumVal a
      (rList, rAccum) = newAccumVal `seq` cascadeMap newAccumVal accumFn decorateFn as
  in  ((decorateFn baseAccumVal a):rList, rAccum)


-- | Splice together segments from different sources depending on the
-- duration they should play for. The input should be a list of 
-- [(source, duration)] for each source that plays for each duration, sequenced
-- together in the order they're in in the list. 
piecewise :: a -> TimeVal -> [(Stream a, TimeVal)] -> Stream TimeVal -> Stream a
piecewise emptyVal startAt sourcesDurations timeSrc = 
  customMap sourcesAbsoluteTimes timeSrc
  where
    sourcesAbsoluteTimes = fst $ cascadeMap startAt 
      (\acc (src, dur) -> acc + dur) 
      (\acc (src, dur) -> (src, dur, acc)) 
      sourcesDurations

    -- Essentially, take from the current source if the current time step covers
    -- its time frame. If the time frame is out, try moving onto the next source
    -- and see if we can consume that one. If time has run out for all the
    -- component sources, we just return a zero-stream. This assumes that the
    -- source-and-time list is sorted in ascending start-time order, which the
    -- sourcesAbsoluteTime definition above should ensure.
    customMap [] _ = pure emptyVal -- we could continue to consume time here but uh
    customMap curr@(((STR.Cons x xs), dur, start):rest) tsrce@(STR.Cons t ts) = 
      if (t < start) 
      then STR.Cons emptyVal (customMap curr ts) -- output empty/silence but still progress forward in time
      else if (t >= start) && (t < start + dur) -- we're in the time frame for this source...
           then STR.Cons x (customMap ((xs, dur, start):rest) ts) -- consume the current source, and the time step
           else customMap rest tsrce -- don't consume this time step yet, try next sources


-- | Convenience wrapper over `piecewise` to compose a source that plays out the
-- sequence of frequencies + volumes
notesToSource :: TimeVal -- ^start offset time of the melody line
              -> Stream TimeVal -- ^time step stream
              -> [Cell] -- ^note data cells making up the melody
              -> (Stream Freq, Stream Vol) -- ^frequency and volume sources
notesToSource startAt ts cells = (fs, vs)
  where
    fs = piecewise 0 startAt ((\(Cell f v d) -> (pure f, d)) <$> cells) ts
    vs = piecewise 0 startAt ((\(Cell f v d) -> (pure v, d)) <$> cells) ts


-- | Consume values from a sample source and produce a list of PCM samples and
-- the remainder of the source
consumeSource :: Int -> SampleSource -> ([SampleVal], SampleSource)
consumeSource = STR.splitAt
