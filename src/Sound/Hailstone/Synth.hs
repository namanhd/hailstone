module Sound.Hailstone.Synth 
( -- * Type synonyms and datatypes
  -- ** Re-exports from "Sound.Hailstone.Types".
  SynthVal, Freq, Vol, Pan, SampleRate, TimeVal, SampleVal
, ChanMode(..)
, Cell(..)
  -- ** Stream types
, Stream
, SampleSource
  -- * Functions
  -- ** For processing a stream at the audio backend level
, consumeSource
, asPCM
  -- ** Creating streams
  -- | Note that thanks to the @Applicative@ instance of `Stream`, the function
  -- `pure` is available to lift any value of type @a@ into @`Stream` a@, which
  -- will infinitely repeat the value.
, timesteps
, waveformSource, sinSource, squareSource
  -- ** Combining and manipulating streams
, clip, mix, mixAll, (|+|), (|*|), scale, (*|)
, constConvolve, varConvolve
, interleave, constStereoize, varStereoize
  -- ** Sequencing streams
, piecewise
, notesToStreams, retriggerWithNotes
)
where

import qualified Data.Stream as STR
import Sound.Hailstone.Types

-- | Re-export from `STR.Stream`; a shorthand for convenience
type Stream = STR.Stream

-- | Type representing streams containing audio sample values, to be consumed by
-- the audio backend
type SampleSource = Stream SampleVal

-- | Create a stream of "time step values" t (for waveform-generating
-- functions), sampled depending on the sample rate
timesteps :: SampleRate -> Stream TimeVal
timesteps sampleRate = ((1/fromIntegral sampleRate) *) <$> STR.fromInfList [0 :: TimeVal ..]

-- | Clip a sample source to the bounds allowed by the sample format.
clip :: SampleSource -> SampleSource
clip = fmap (min (maxBound :: SampleVal)) . fmap (max (minBound :: SampleVal))

-- | Scale a stream by a multiplier.
scale :: (Num a) => a -> Stream a -> Stream a
scale multiplier = fmap (multiplier *)

-- | Infix version of `scale`.
(*|) :: (Num a) => a -> Stream a -> Stream a
(*|) = scale
infixr 5 *|

-- | Create a stream by convolving (sliding-window weighted average) over an
-- existing stream. Effectively a low-pass filter, but one can specify weights
-- for the convolution filter. The list of weights should have (technically at
-- least, but ideally exactly) @windowSize@ elements. The bigger the window, the
-- "smoother" the signal becomes, though it also depends on the weights. This is
-- `constConvolve` to contrast with `varConvolve` whose parameters are
-- modulatable via Streams. The interface for that is quite a bit more hefty
-- (and constConvolve is already convoluted enough (hah)) so I've separated the
-- two functions.
constConvolve :: (Num a) => Int -> [a] -> Stream a -> Stream a
constConvolve wndSize weights src = let window = fst $ STR.splitAt wndSize src in
  STR.cons (sum $ zipWith (*) weights window) 
    (constConvolve wndSize weights $ STR.tail src)

-- | Like `constConvolve` but the window size and conv weight lists are also
-- streams.
varConvolve :: (Num a) => Stream Int -> Stream [a] -> Stream a -> Stream a
varConvolve wndStr wtStr src = 
  -- varConvolve ~(STR.Cons wndSize wnds) ~(STR.Cons weights wts) ~src@(STR.Cons _ xs) = 
  -- STR.Cons (sum $ zipWith (*) weights window) (varConvolve wnds wts xs)
  STR.cons (sum $ zipWith (*) weights window) (varConvolve wnds wts xs)
  where
    ~(wndSize, wnds) = STR.headtail wndStr
    ~(weights, wts) = STR.headtail wtStr
    xs = STR.tail src
    window = fst $ STR.splitAt wndSize src

-- | Mix two streams with addition.
mix :: (Num a) => Stream a -> Stream a -> Stream a
mix s1 s2 = STR.zipWith (+) s1 s2

-- | Infix version of `mix`.
(|+|) :: (Num a) => Stream a -> Stream a -> Stream a
(|+|) = mix
infixl 3 |+|

-- | Since we have `|+|`, we might as well have `|*|` which multiplies two 
-- streams pointwise.
(|*|) :: (Num a) => Stream a -> Stream a -> Stream a
(|*|) s1 s2 = STR.zipWith (*) s1 s2
infixl 4 |*|

-- | Interleaves two streams together e.g. to make a stereo signal. Re-exports
-- the same function from "Data.Stream".
interleave :: (Num a) => Stream a -> Stream a -> Stream a
interleave = STR.interleave

-- | Mix a list of streams.
mixAll :: (Num a) => [Stream a] -> Stream a
mixAll = foldl1 mix

-- | Convert a double-generating source (such as a raw waveform stream) into a
-- stream of audio sample values in our audio setup (here, @Int16@), with
-- scaling up to sample values followed by rounding and clipping
asPCM :: Stream SynthVal -> SampleSource
asPCM = clip . fmap round . scale (fromIntegral (maxBound :: SampleVal))

-- | Interleave a stream with itself and apply some panning to turn a mono
-- stream into a stereo stream. Note that pan values are from 0. to 1., where
-- 0.0 is hard left, 0.5 is centered, and 1.0 is hard right. (Though, no
-- bounds checking is done to ensure this.)
constStereoize :: (Num a) => a -> Stream a -> Stream a
constStereoize pan s = interleave (scale (1 - pan) s) (scale pan s)

-- | The stream-parameterized version of `constStereoize`, where pan values 
-- may come in a stream rather than as a constant. This requires some special
-- swizzling and we can't just use the stock interleave function.
varStereoize :: (Num a) => Stream a -> Stream a -> Stream a
varStereoize pans src = STR.cons ((1 - p) * x) $ STR.cons (p * x) $ 
  varStereoize ps xs
  where
    (p, ps) = STR.headtail pans
    (x, xs) = STR.headtail src  

-- | Generic waveform source (a waveform function that can be used is any
-- @(SynthVal -> SynthVal)@ function with domain [0, inf) and range [-1, 1]).
waveformSource :: (SynthVal -> SynthVal) -> Stream Freq -> Stream Vol -> Stream TimeVal -> Stream SynthVal
waveformSource waveFn = STR.zipWith3 (\f v t -> v * waveFn (f * t))

-- | Sinusoidal waveform source; supports modulation of phase, frequency,
-- volume, and time scale.
sinSource :: Stream SynthVal -> Stream Freq -> Stream Vol -> Stream TimeVal -> Stream SynthVal
sinSource = -- TODO use a fastSin instead of this maybe 
  STR.zipWith4 (\ph f v t -> v * sin (2 * pi * f * t + ph))

-- | Square waveform source; supports modulation of frequency, volume, and time
-- scale. (Duty cycle might become a parameter later down the line)
squareSource :: Stream Freq -> Stream Vol -> Stream TimeVal -> Stream SynthVal
squareSource = STR.zipWith3 (\f v t -> let ft = f * t in v * fromIntegral 
  ((2 * (2 * floor ft - floor (2 * ft)) + 1) :: Int))


-- | Funny @fold@/@mapAccum@-ish helper for `piecewise`, but may be useful
-- elsewhere. Carries out a foldl with a base accumulator value and an
-- accumulator function but also takes a "decorator function" as argument to
-- combine the current list item with the current accumulator value and obtain
-- an item in the output list (the type @b@ in the function's type.) Common use
-- case is to "decorate" the list with the accumulator value alongside folding.
-- Outputs both the accumulator result and the resulting decorated list.
-- Example usage:
-- >>> cascadeMap 0 (\acc (c, n) -> acc + n) (\acc (c, n) -> (c, n, acc)) 
-- >>>    $ [('a',1),('b',5),('c',3)]
-- ([('a',1,0),('b',5,1),('c',3,6)],9)
cascadeMap :: x -> (x -> a -> x) -> (x -> a -> b) -> [a] -> ([b], x)
cascadeMap baseAccumVal _ _ [] = ([], baseAccumVal)
cascadeMap baseAccumVal accumFn decorateFn (a:as) = 
  let newAccumVal = accumFn baseAccumVal a
      (rList, rAccum) = newAccumVal `seq` cascadeMap newAccumVal accumFn decorateFn as
  in  ((decorateFn baseAccumVal a):rList, rAccum)


-- | Splice together segments from different sources depending on the duration
-- they should play for. The input should be a list of @[(source, duration)]@
-- for each source that plays for each duration, sequenced together in the order
-- they're in in the list. 
piecewise :: ChanMode -> a -> TimeVal -> [(Stream a, TimeVal)] -> Stream TimeVal -> Stream a
piecewise chanMode emptyVal startAt sourcesDurations timeSrc = 
  customMap sourcesAbsoluteTimes timeSrcAtChanMode
  where
    timeSrcAtChanMode = case chanMode of
      Mono -> timeSrc
      Stereo -> interleave timeSrc timeSrc
    sourcesAbsoluteTimes = fst $ cascadeMap startAt 
      (\acc (_  , dur) -> acc + dur) 
      (\acc (src, dur) -> (src, dur, acc)) 
      sourcesDurations

    -- Essentially, take from the current source if the current time step covers
    -- its time frame. If the time frame is out, try moving onto the next source
    -- and see if we can consume that one. If time has run out for all the
    -- component sources, we just return a zero-stream. This assumes that the
    -- source-and-time list is sorted in ascending start-time order, which the
    -- sourcesAbsoluteTime definition above should ensure.
    customMap [] _ = pure emptyVal -- we could continue to consume time here but uh
    customMap ~curr@((str, dur, start):rest) tsrce = 
      let ~(t, ts) = STR.headtail tsrce
          ~(x, xs) = STR.headtail str
      in 
      if (t < start) 
      then STR.cons emptyVal (customMap curr ts) -- output empty/silence but still progress forward in time
      else if (t >= start) && (t < start + dur) -- we're in the time frame for this source...
           then STR.cons x (customMap ((xs, dur, start):rest) ts) -- consume the current source, and the time step
           else customMap rest tsrce -- don't consume this time step yet, try next sources


-- | Convenience wrapper over `piecewise` to compose a source that plays out the
-- sequence of frequencies + volumes according to some note data (as `Cell`s)
notesToStreams :: ChanMode 
               -> TimeVal -- ^start offset time of the melody line
               -> Stream TimeVal -- ^time step stream
               -> [Cell] -- ^note data cells making up the melody
               -> (Stream Freq, Stream Vol, Stream Pan) -- ^frequency, volume, panning streams
notesToStreams chanMode startAt ts cells = (fs, vs, ps)
  where
    go mapper = piecewise chanMode 0 startAt (mapper <$> cells) ts
    fs = go (\cell -> (pure (freqOf cell), durOf cell))
    vs = go (\cell -> (pure (volOf  cell), durOf cell))
    ps = case chanMode of 
      Mono -> pure 0.5 -- because the user passed in Mono/Stereo, they must know
      -- to not have any use for this pure 0.5 stream... unless they want to use
      -- this in an otherwise stereo mix of sources?
      Stereo -> go (\cell -> (pure . unmaybePan $ panOf cell, durOf cell))

-- | This is what is probably most commonly understood as "playing a melody with
-- a synth". By default, simply modulating an "instrument" stream's frequency
-- with the frequency and volume streams created from `notesToStreams` will
-- simply superimpose the melody on top of the carrier sound, which may evolve
-- independently of the note frequencies. In order to achieve a
-- "keystroke"/"instrument retrigger" effect, this function allows one to "reset
-- and retrigger" a stream in sync with note data. This will work on any streams
-- that are parameterized by a frequency, volume, and timestep (i.e. a synth
-- source); in addition, panning will be applied if stereo output is desired.
retriggerWithNotes  :: ChanMode -- ^specify `Mono` or `Stereo`; Mono will ignore the `Cell`s' `panOf` value.
                    -> SynthVal -- ^empty value for when notes have concluded
                    -> TimeVal -- ^start offset time of the melody line
                    -> Stream TimeVal -- ^time step stream
                    -> (Stream Freq -> Stream Vol -> Stream TimeVal -> Stream SynthVal) 
                        -- ^stream to retrigger in sync with notes, parameterized
                        -- by frequency, volume, and timestep 
                    -> [Cell] -- ^note data cells making up the melody
                    -> Stream SynthVal
retriggerWithNotes chanMode emptyVal startAt ts sfunc cells = 
  piecewise chanMode emptyVal startAt 
    ((\cell -> ( doPanning (panOf cell) $ sfunc (pure $ freqOf cell) (pure $ volOf cell) ts
      , durOf cell )) <$> cells) ts
  where
    doPanning maybePan = case chanMode of
      Mono -> id
      Stereo -> constStereoize $ unmaybePan maybePan

-- | Consume values from a sample source and produce a list of PCM samples and
-- the remainder of the source
consumeSource :: Int -> SampleSource -> ([SampleVal], SampleSource)
consumeSource = STR.splitAt
