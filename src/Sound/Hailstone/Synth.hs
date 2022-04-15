module Sound.Hailstone.Synth
( -- * Type synonyms and datatypes
  -- ** Re-exports from "Sound.Hailstone.Types".
  SynthVal, Freq, Vol, Pan, SampleRate, TimeVal, SampleVal
, ChanMode(..)
, Cell(..)
  -- ** Signal types
, Signal
, SampleSource
, CurrChan(..)
  -- * Functions
  -- ** For processing a stream at the audio backend level
, sampleSignalAt
, asPCM
  -- ** Creating signals
, waveformSource, sinSource, squareSource
  -- ** Combining and manipulating signals
, clip, mix, mixAll, (|+|), (|*|), scale, (*|)
, constConvolve, varConvolve, filterLP
, interleave, constStereoize, varStereoize, delay, modifyTime
  -- ** Sequencing streams
, piecewise
, retriggerWithNotes
)
where

-- A better Synth module and abstraction, this time no longer using lazy lists,
-- but using... Reader instead! it really is just actually the (r->) monad.
import Control.Monad.Reader
import Sound.Hailstone.Types

-- | The output of a signal depends on the current time, the sampling delta,
-- and the current channel (L/R/Mono).
type Signal = Reader (TimeVal, TimeVal, CurrChan)

data CurrChan = CMono | CLeft | CRight
  deriving (Eq, Show)

-- | A signal function will need access to the current time tick, as well as the
-- time-delta between ticks (i.e. 1 / (sample-rate)), as well as the channel
-- being played-on (for interleaving). 
type SampleSource = Signal SampleVal

-- | Retrieve the time tick and time delta context from the Reader monad
getTime :: Signal TimeVal
getTime = ask >>= \(t,_, _) -> pure t

-- | Synonym of `runReader` for readability
runSignal :: Signal a -> (TimeVal, TimeVal, CurrChan) -> a
runSignal = runReader

-- | Clip a sample source to the bounds allowed by the sample format.
clip :: SampleSource -> SampleSource
clip = fmap (min (maxBound :: SampleVal) . max (minBound :: SampleVal))

-- | Scale a signal by a multiplier.
scale :: (Num a) => a -> Signal a -> Signal a
scale multiplier = fmap (multiplier *)

-- | Infix version of `scale`.
(*|) :: (Num a) => a -> Signal a -> Signal a
(*|) = scale
infixr 5 *|

-- | Create a signal by convolving (sliding-window weighted average) over an
-- existing signal. Effectively a low-pass filter, but one can specify weights
-- for the convolution filter. The list of weights should have (technically at
-- least, but ideally exactly) @windowSize@ elements. The bigger the window, the
-- "smoother" the signal becomes, though it also depends on the weights. This is
-- `constConvolve` to contrast with `varConvolve` whose parameters are
-- modulatable via Signals. The interface for that is quite a bit more hefty
-- (and constConvolve is already convoluted enough (hah)) so I've separated the
-- two functions.
constConvolve :: (Num a) => Int -> [a] -> Signal a -> Signal a
constConvolve wndSize weights sig = ask >>= \(t, d, c) ->
  pure . sum . zipWith (*) weights $ 
    runSignal sig <$> [(t + d * i, d, c) | i <- [0..fromIntegral wndSize]]


-- | Like `constConvolve` but the window size and conv weight lists are also
-- time-varying signals
varConvolve :: (Num a) => Signal Int -> Signal [a] -> Signal a -> Signal a
varConvolve windowSig weightSig sig = do
  (t, d, c) <- ask
  wndSize <- windowSig
  weights <- weightSig
  pure . sum . zipWith (*) weights $ 
    runSignal sig <$> [(t + d*i, d, c) | i <- [0..fromIntegral wndSize]]

-- | Convenience wrapper for constConvolve for common LP filtering.
filterLP :: (Num a, Fractional a) => Int -> Signal a -> Signal a
filterLP wndSize = constConvolve wndSize (windowWeights wndSize)
  where 
    windowWeights windowSize = fmap (* (1/fromIntegral windowSize)) 
      . take windowSize $ repeat 1


-- | Mix two signals with addition.
mix :: (Num a) => Signal a -> Signal a -> Signal a
mix = liftM2 (+) -- ok <*> might be slow.. but do we even have a better solution

-- | Infix version of `mix`.
(|+|) :: (Num a) => Signal a -> Signal a -> Signal a
(|+|) = mix
infixl 3 |+|

-- | Since we have `|+|`, we might as well have `|*|` which multiplies two 
-- signals pointwise.
(|*|) :: (Num a) => Signal a -> Signal a -> Signal a
(|*|) = liftM2 (*)
infixl 4 |*|

-- | Interleaves two signals together to make a stereo signal.
interleave :: (Num a) => Signal a -> Signal a -> Signal a
interleave s1 s2 = ask >>= \(_, _, c) -> case c of
  CLeft -> s1
  CRight -> s2
  CMono -> s1 |+| s2 -- ???? TODO work out if this is right lol

-- | Warps/modifies the time axis for a signal using a function.
modifyTime :: (TimeVal -> TimeVal) -> Signal a -> Signal a
modifyTime f = local (\(t, d, c) -> (f t, d, c))

-- | Delay a signal by some duration, padding in front with an empty value.
delay :: a -> TimeVal -> Signal a -> Signal a
delay emptyVal dur sig = getTime >>= \t -> if t < dur then pure emptyVal else 
  modifyTime (subtract dur) sig

-- | Mix a list of streams.
mixAll :: (Num a) => [Signal a] -> Signal a
mixAll = foldl1 mix

-- | Convert a double-generating source (such as a raw waveform stream) into a
-- stream of audio sample values in our audio setup (here, @Int16@), with
-- scaling up to sample values followed by rounding and clipping
asPCM :: Signal SynthVal -> SampleSource
asPCM = clip . fmap round . scale (fromIntegral (maxBound :: SampleVal))

-- | Interleave a stream with itself and apply some panning to turn a mono
-- stream into a stereo stream. Note that pan values are from 0. to 1., where
-- 0.0 is hard left, 0.5 is centered, and 1.0 is hard right. (Though, no
-- bounds checking is done to ensure this.)
constStereoize :: (Num a) => a -> Signal a -> Signal a
constStereoize pan s = interleave (scale (1 - pan) s) (scale pan s)

-- | The signal-parameterized version of `constStereoize`, where pan values 
-- may come in a signal rather than as a constant.
varStereoize :: (Num a) => Signal a -> Signal a -> Signal a
varStereoize panSig src = 
  panSig >>= \pan -> interleave (scale (1 - pan) src) (scale pan src)

-- | Generic waveform source (a waveform function that can be used is any
-- @(SynthVal -> SynthVal)@ function with domain [0, inf) and range [-1, 1]).
waveformSource :: (SynthVal -> SynthVal) -> Signal Freq -> Signal Vol -> Signal SynthVal
waveformSource waveFn fs vs = 
  getTime >>= \t -> liftM2 (\f v -> v * waveFn (f * t)) fs vs

-- | Sinusoidal waveform source
sinSource :: Signal SynthVal -> Signal Freq -> Signal Vol -> Signal SynthVal
sinSource ps fs vs = 
  getTime >>= \t -> liftM3 (\p f v -> v * sin (2 * pi * f * t + p)) ps fs vs
  

-- | Square waveform source; supports modulation of frequency, volume, and time
-- scale. (Duty cycle might become a parameter later down the line)
squareSource :: Signal Freq -> Signal Vol -> Signal SynthVal
squareSource fs vs = getTime >>= \t -> liftM2 (\f v -> let ft = f * t in
  v * fromIntegral ((2 * (2 * floor ft - floor (2 * ft)) + 1) :: Int)) fs vs


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
piecewise :: a -> [(Signal a, TimeVal)] -> Signal a
piecewise emptyVal sourcesDurations = getTime >>= customMap sourcesAbsoluteTimes
  where
    -- timeSrcAtChanMode = case chanMode of
    --   Mono -> timeSrc
    --   Stereo -> interleave timeSrc timeSrc
    sourcesAbsoluteTimes = fst $ cascadeMap (0.0 :: TimeVal)
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
    customMap ((sig, dur, start):rest) t
      | (t < start) = pure emptyVal
      | (t >= start) && (t < start + dur) = modifyTime (subtract start) sig
      | otherwise = customMap rest t

{-
-- | Convenience wrapper over `piecewise` to compose a source that plays out the
-- sequence of frequencies + volumes according to some note data (as `Cell`s)
notesToSignals :: TimeVal -- ^start offset time of the melody line
               -> [Cell] -- ^note data cells making up the melody
               -> (Signal Freq, Signal Vol, Signal Pan) -- ^frequency, volume, panning streams
notesToSignals startAt cells = (fs, vs, ps)
  where
    go mapper = piecewise 0 startAt (mapper <$> cells)
    fs = go (\cell -> (pure (freqOf cell), durOf cell))
    vs = go (\cell -> (pure (volOf  cell), durOf cell))
    ps = case chanMode of 
      Mono -> pure 0.5 -- because the user passed in Mono/Stereo, they must know
      -- to not have any use for this pure 0.5 stream... unless they want to use
      -- this in an otherwise stereo mix of sources?
      Stereo -> go (\cell -> (pure . unmaybePan $ panOf cell, durOf cell))
-}

-- | This is what is understood as "playing a melody with a synth". Effectively
-- "resets and retriggers" a signal in sync with note data. This will work on
-- any signal that is parameterized by a frequency and volume (i.e. a synth
-- source); in addition, panning will be applied if stereo output is desired.
retriggerWithNotes  :: SynthVal -- ^empty value for when notes have concluded
                    -> (Signal Freq -> Signal Vol -> Signal SynthVal) 
                        -- ^stream to retrigger in sync with notes, parameterized
                        -- by frequency, volume, and timestep 
                    -> [Cell] -- ^note data cells making up the melody
                    -> Signal SynthVal
retriggerWithNotes emptyVal sfunc cells = 
  piecewise emptyVal 
    ((\cell -> (doPanning (panOf cell) $ sfunc (pure $ freqOf cell) (pure $ volOf cell)
      , durOf cell )) <$> cells)
  where
    doPanning maybePan = constStereoize $ unmaybePan maybePan


-- | Actually run and sample from the signal at some time, delta, and channel.
sampleSignalAt :: TimeVal -> TimeVal -> CurrChan -> SampleSource -> SampleVal
sampleSignalAt t d c sig = runSignal sig (t, d, c)