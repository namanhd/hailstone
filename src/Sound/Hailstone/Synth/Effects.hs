{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Sound.Hailstone.Synth.Effects
( -- * Effects
  gain2ampl
, gain, gainN
, cents2ratio, ratio2cents, nCents2ratio, nRatio2cents
, echoRaw, echo, echo'
  -- ** Filters à la RBJ's audio EQ cookbook
, lpf, lpfN
)
where

import Prelude hiding (isNaN)
import Sound.Hailstone.Synth.Node
import qualified Sound.Hailstone.Synth.DelayQueue as DelayQueue

-- Effects must be either polymorphic or at least be specialized to @`LR` `SynthVal`@ rather
-- than just SynthVal.

-- | Convert a gain value (in decibels) to amplitude, equal to @10**(dBgain/20)@.
gain2ampl :: Floating a => a -> a
gain2ampl g = 10.0 ** (0.05 * g)
{-# INLINABLE gain2ampl #-}

-- | `gain2ampl` specialized for nodes, spawning fewer nodes.
nGain2ampl :: Floating a => Node e a -> Node e a
nGain2ampl = fmap gain2ampl
{-# INLINABLE nGain2ampl #-}

-- | Apply constant gain (given in dB) to a node.
gain :: Floating a => a -> Node e a -> Node e a
gain g ~node = let a = gain2ampl g in a *| node
{-# INLINABLE gain #-}

-- | Apply variable gain (given in dB, emitted from a node) to a node.
gainN :: Floating a => Node e a -> Node e a -> Node e a
gainN g ~node = let a = nGain2ampl g in a * node
{-# INLINABLE gainN #-}

-- | Convert cents to frequency ratio.
cents2ratio :: Floating a => a -> a
cents2ratio cents = 2 ** (cents / 1200)
{-# INLINABLE cents2ratio #-}

-- | Convert a frequency ratio to cents.
ratio2cents :: Floating a => a -> a
ratio2cents ratio = 1200 * logBase 2 ratio
{-# INLINABLE ratio2cents #-}

nCents2ratio :: Floating a => Node e a -> Node e a
nCents2ratio = fmap cents2ratio
{-# INLINABLE nCents2ratio #-}

nRatio2cents :: Floating a => Node e a -> Node e a
nRatio2cents = fmap ratio2cents
{-# INLINABLE nRatio2cents #-}

-- *** Filters à la RBJ's EQ cookbook

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

_cookbookFilter_fn :: CookbookFilterCoeffs -> CookbookFilterState -> LR SynthVal -> CookbookFilterState
_cookbookFilter_fn (MkCFCoeffs a0inv a1 a2 b0 b1 b2) (MkCFState xm1 xm2 _ ym1 ym2 _) x = let
  y = a0inv .*: ((b0 .*: x) + (b1 .*: xm1) + (b2 .*: xm2) - (a1 .*: ym1) - (a2 .*: ym2))
  in MkCFState x xm1 xm2 y ym1 ym2

_cookbookFilter_nanCoeffs :: CookbookFilterCoeffs
_cookbookFilter_nanCoeffs = MkCFCoeffs nan nan nan nan nan nan

_cookbookFilter_zeroState :: CookbookFilterState
_cookbookFilter_zeroState = MkCFState 0 0 0 0 0 0

_cookbookFilter_s0 :: SPair CookbookFilterCoeffs CookbookFilterState
_cookbookFilter_s0 = MkS2 _cookbookFilter_nanCoeffs _cookbookFilter_zeroState

_cookbookFilterN_s0 :: STriple Freq CookbookFilterCoeffs CookbookFilterState
_cookbookFilterN_s0 = MkS3 nan _cookbookFilter_nanCoeffs _cookbookFilter_zeroState

_cookbookFilter_SF :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> Freq -> SigEnv e -> LR SynthVal -> SPair CookbookFilterCoeffs CookbookFilterState -> SPair (LR SynthVal) (SPair CookbookFilterCoeffs CookbookFilterState)
_cookbookFilter_SF coeffsFn q f (_, d, _) x (MkS2 savedCoeffs savedState) = let
  c = if isNaN savedCoeffs.a0inv then coeffsFn d q f else savedCoeffs
  s = _cookbookFilter_fn c savedState x
  in MkS2 s.ym0 (MkS2 c s)

_cookbookFilterN_SF :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> SigEnv e -> Freq -> LR SynthVal -> STriple Freq CookbookFilterCoeffs CookbookFilterState -> SPair (LR SynthVal) (STriple Freq CookbookFilterCoeffs CookbookFilterState)
_cookbookFilterN_SF coeffsFn q (_, d, _) thisFreq x (MkS3 lastFreq savedCoeffs savedState) = let
  c = if thisFreq == lastFreq then savedCoeffs else coeffsFn d q thisFreq
  s = _cookbookFilter_fn c savedState x
  in MkS2 s.ym0 (MkS3 thisFreq c s)

cookbookCoeffsFn_LPF :: TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs
cookbookCoeffsFn_LPF d q f = let
  omega = twopi * f * d
  cosom = cos omega
  sinom = sin omega
  alpha = sinom / (2 * q)
  b0 = (1 - cosom) / 2
  in MkCFCoeffs
    { a0inv = recip (1 + alpha)
    , a1 = -2 * cosom
    , a2 = 1 - alpha
    , b0 = b0
    , b1 = 1 - cosom
    , b2 = b0
    }

-- | RBJ 2nd-order filter. <https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html>
-- Takes a coefficients-calculating function (which itself takes delta time, Q, frequency.)
cookbookFilter :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
cookbookFilter coeffsFn q f = mkNode1 _cookbookFilter_s0 (_cookbookFilter_SF coeffsFn q f)

-- | `cookbookFilter` but with variable filter frequency from a node.
cookbookFilterN :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> Node e Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
cookbookFilterN coeffsFn q = MkNode2 _cookbookFilterN_s0 (_cookbookFilterN_SF coeffsFn q)

-- | 2nd-order low-pass filter.
lpf :: SynthVal -> Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
lpf = cookbookFilter cookbookCoeffsFn_LPF

-- | `lpf` but with variable filter frequency.
lpfN :: SynthVal -> Node e Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
lpfN = cookbookFilterN cookbookCoeffsFn_LPF

-- *** Delay, echo

-- | Single-stream echo with constant delay time, decay multiplier, wet mix level (0-1), low
-- pass filter Q factor, cutoff frequency, and pan applied to the wet signal.
--
-- (More efficient, fused implementation of `echo`.)
echo' :: TimeVal -> Ampl -> Ampl -> SynthVal -> Freq -> Pan -> Node e (LR SynthVal) -> Node e (LR SynthVal)
echo' delayMs decayMult wetLvl filterQ filterF wetPan = let delaySec = delayMs * 0.001; dryLvl = max 0 (1 - wetLvl)
  in mkNode1IO (MkS2 DelayQueue.delay_s0 _cookbookFilter_s0) $ \r@(_, d, _) x (MkS2 myDQ filterState) ->
    DelayQueue.withDelay delaySec decayMult d myDQ x $ \dq delayOut _ -> let
      (MkS2 filteredDelayOut filterState') = _cookbookFilter_SF
        cookbookCoeffsFn_LPF filterQ filterF r delayOut filterState
      in pure $ MkS2 ((dryLvl .*: x) + balanceLR wetPan (wetLvl .*: filteredDelayOut))
        (MkS2 dq filterState')

-- | Single-stream echo with constant delay time and decay multiplier. This produces the raw
-- echo wet signal; do your own mixing and filtering afterwards (or use `echo`.)
echoRaw :: TimeVal -> Ampl -> Node e (LR SynthVal) -> Node e (LR SynthVal)
echoRaw delayMs decayMult = let delaySec = delayMs * 0.001
  in mkNode1IO DelayQueue.delay_s0 $ \(_, d, _) x myDQ ->
    DelayQueue.withDelay delaySec decayMult d myDQ x $ \dq delayOut _ -> pure $ MkS2 delayOut dq

-- | Single-stream echo with constant delay time, decay multiplier, wet mix level (0-1), low
-- pass filter Q factor, cutoff frequency, and pan applied to the wet signal.
--
-- Less efficient but more readable and extensible implementation of `echo'`.
echo :: TimeVal -> Ampl -> Ampl -> SynthVal -> Freq -> Pan -> Node e (LR SynthVal) -> Node e (LR SynthVal)
echo delayMs decayMult wetLvl filterQ filterF wetPan input = output
  where
    input' = share input
    filteredDelayOut = lpf filterQ filterF $ echoRaw delayMs decayMult input'
    output = (1 - wetLvl) *|| input' + (balance wetPan $ wetLvl *|| filteredDelayOut)
