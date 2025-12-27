{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sound.Hailstone.Synth.Effect
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
import Data.Functor ((<&>))
import Sound.Hailstone.Synth.Node
import qualified Sound.Hailstone.Synth.DelayQueue as DQ

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

_cookbookFilter_s0 :: CookbookFilterState
_cookbookFilter_s0 = _cookbookFilter_zeroState

_cookbookFilterN_s0 :: STriple Freq CookbookFilterCoeffs CookbookFilterState
_cookbookFilterN_s0 = MkS3 nan _cookbookFilter_nanCoeffs _cookbookFilter_zeroState

_cookbookFilter_SF :: CookbookFilterCoeffs -> SigEnv e -> LR SynthVal -> CookbookFilterState -> SPair (LR SynthVal) CookbookFilterState
_cookbookFilter_SF c _ x savedState = let
  s = _cookbookFilter_fn c savedState x
  in MkS2 s.ym0 s

_cookbookFilterN_SF :: TimeVal -> (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> SigEnv e -> Freq -> LR SynthVal -> STriple Freq CookbookFilterCoeffs CookbookFilterState -> SPair (LR SynthVal) (STriple Freq CookbookFilterCoeffs CookbookFilterState)
_cookbookFilterN_SF d coeffsFn q _ thisFreq x (MkS3 lastFreq savedCoeffs savedState) = let
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
cookbookFilter
  :: MonadHasDeltaTime m
  => (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs)
  -> SynthVal -- ^filter Q
  -> Freq -- ^filter frequency
  -> Node e (LR SynthVal) -- ^input node
  -> m (Node e (LR SynthVal))
cookbookFilter coeffsFn q f input = askDeltaTime <&> \d -> mkNode1 _cookbookFilter_s0 (_cookbookFilter_SF (coeffsFn d q f)) input

-- | `cookbookFilter` but with time-variable filter frequency from a node.
cookbookFilterN
  :: MonadHasDeltaTime m
  => (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs)
  -> SynthVal -- ^filter Q
  -> Node e Freq -- ^node of time-varying filter frequency
  -> Node e (LR SynthVal) -- ^input node
  -> m (Node e (LR SynthVal))
cookbookFilterN coeffsFn q fNode input = askDeltaTime <&> \d -> MkNode2 _cookbookFilterN_s0 (_cookbookFilterN_SF d coeffsFn q) fNode input

-- | 2nd-order low-pass filter.
lpf :: MonadHasDeltaTime m => SynthVal -> Freq -> Node e (LR SynthVal) -> m (Node e (LR SynthVal))
lpf = cookbookFilter cookbookCoeffsFn_LPF

-- | `lpf` but with variable filter frequency.
lpfN :: MonadHasDeltaTime m => SynthVal -> Node e Freq -> Node e (LR SynthVal) -> m (Node e (LR SynthVal))
lpfN = cookbookFilterN cookbookCoeffsFn_LPF

-- *** Delay, echo

-- | Single-stream echo with constant delay time, decay multiplier, wet mix level (0-1), low
-- pass filter Q factor, cutoff frequency, and pan applied to the wet signal.
--
-- (More efficient, fused implementation of `echo`.)
echo' :: (MonadHasDeltaTime m, DQ.MonadHasDelayQueue m)
      => TimeVal -- ^delay in milliseconds
      -> Ampl -- ^feedback multiplier
      -> Ampl -- ^wet mix level
      -> SynthVal -- ^filter Q
      -> Freq -- ^filter frequency
      -> Pan -- ^pan applied to filtered, wet-level-scaled wet signal
      -> Node e (LR SynthVal) -- ^input node
      -> m (Node e (LR SynthVal))
echo' delayMs decayMult wetLvl filterQ filterF wetPan input = do
  d <- askDeltaTime
  let delaySec = delayMs * 0.001
      dryLvl = max 0 (1 - wetLvl)
      filterCoeffs = cookbookCoeffsFn_LPF d filterQ filterF
  dq0 <- DQ.initDQ delaySec d zeroLR
  pure $ flip (mkNode1IO (MkS2 dq0 _cookbookFilter_s0)) input $ \r x (MkS2 myDQ filterState) ->
    DQ.withDelay decayMult myDQ x $ \dq delayOut _ -> let
      (MkS2 filteredDelayOut filterState') = _cookbookFilter_SF filterCoeffs r delayOut filterState
      in pure $ MkS2 ((dryLvl .*: x) + balanceLR wetPan (wetLvl .*: filteredDelayOut)) (MkS2 dq filterState')

-- | Single-stream echo with constant delay time and decay multiplier. This produces the raw
-- echo wet signal; do your own mixing and filtering afterwards (or use `echo`.)
echoRaw :: (MonadHasDeltaTime m, DQ.MonadHasDelayQueue m)
        => TimeVal -- ^delay in milliseconds
        -> Ampl -- ^feedback multiplier
        -> Node e (LR SynthVal) -- ^input node
        -> m (Node e (LR SynthVal))
echoRaw delayMs decayMult input = do
  let delaySec = delayMs * 0.001
  d <- askDeltaTime
  dq0 <- DQ.initDQ delaySec d zeroLR
  pure $ flip (mkNode1IO dq0) input $ \_ x myDQ ->
    DQ.withDelay decayMult myDQ x $ \dq delayOut _ -> pure $ MkS2 delayOut dq

-- | Single-stream echo with constant delay time, decay multiplier, wet mix level (0-1), low
-- pass filter Q factor, cutoff frequency, and pan applied to the wet signal.
--
-- Less efficient but more readable and extensible implementation of `echo'`.
echo  :: (MonadHasDeltaTime m, MonadHasNodeSharing m, DQ.MonadHasDelayQueue m)
      => TimeVal -- ^delay in milliseconds
      -> Ampl -- ^feedback multiplier
      -> Ampl -- ^wet mix level
      -> SynthVal -- ^filter Q
      -> Freq -- ^filter frequency
      -> Pan -- ^pan applied to filtered, wet-level-scaled wet signal
      -> Node e (LR SynthVal) -- ^input node
      -> m (Node e (LR SynthVal))
echo delayMs decayMult wetLvl filterQ filterF wetPan input = do
  input' <- share input
  filteredDelayOut <- lpf filterQ filterF =<< echoRaw delayMs decayMult input'
  let output = (1 - wetLvl) *|| input' + (balance wetPan $ wetLvl *|| filteredDelayOut)
  pure output
