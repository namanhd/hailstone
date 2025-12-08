{-# LANGUAGE Strict #-}

module Sound.Hailstone.Synth.Generator
( -- * Generators
sinOsc, sinOscPM, sinOscFbkPM, sqrOsc, sqrOscDM, sawOsc, triOsc, triOscPM, sawOscSM
)
where

import Sound.Hailstone.Synth.Node

-- | generic oscillator function with \"phase\" accumulator (but \"phase\" is from 0 to 1)
_osc_fn :: TimeVal -> Freq -> Percent -> (Percent -> SynthVal) -> SPair SynthVal Percent
_osc_fn d f myProgress k = let
  newProgress' = myProgress + f * d
  newProgress = if newProgress' > 1 then newProgress' - 1 else newProgress'
  in MkS2 (k newProgress) newProgress
  -- progress is from 0 to 1 (assuming f is never negative to not have to check <0...)
{-# INLINE _osc_fn #-}

-- | oscillator with feedback with a 1-sample delay (stores last output and
-- pipes it back into the continuation for the next output sample)
_osc_fbk_fn :: TimeVal -> Freq -> (SPair Percent SynthVal) -> (Percent -> SynthVal -> SynthVal) -> SPair SynthVal (SPair Percent SynthVal)
_osc_fbk_fn d f (MkS2 myProgress myLastOutput) k = let
  newProgress' = myProgress + f * d
  newProgress = if newProgress' > 1 then newProgress' - 1 else newProgress'
  out = k newProgress myLastOutput
  in MkS2 out (MkS2 newProgress out)
{-# INLINE _osc_fbk_fn #-}

_osc_s0 :: Percent
_osc_s0 = 0.0
{-# INLINE _osc_s0 #-}

_osc_fbk_s0 :: SPair Percent SynthVal
_osc_fbk_s0 = MkS2 0.0 0.0
{-# INLINE _osc_fbk_s0 #-}

---------- sin oscillator with adjustable phase addition
_sinOsc_fn :: TimeVal -> Ampl -> Freq -> Phase -> Percent -> SPair SynthVal Percent
_sinOsc_fn d a f phase s = _osc_fn d f s $ \x -> a * sin (twopi * x + phase)

_sinOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_sinOsc_SF (MkSigEnv { d = d }) a f = _sinOsc_fn d a f 0

_sinOscPM_SF :: SigEnv e -> Ampl -> Freq -> Phase -> Percent -> SPair SynthVal Percent
_sinOscPM_SF (MkSigEnv { d = d }) a f phase = _sinOsc_fn d a f phase

_sinOscFbkPM_SF :: SigEnv e -> Ampl -> Freq -> Phase -> Ampl -> (SPair Percent SynthVal) -> SPair SynthVal (SPair Percent SynthVal)
_sinOscFbkPM_SF (MkSigEnv { d = d }) a f phase fbkLvl s = _osc_fbk_fn d f s $
  \x fbk -> a * sin (twopi * x + phase + fbkLvl * fbk)

-- | Sine oscillator taking amplitude and frequency as input nodes.
sinOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sinOsc = MkNode2 _osc_s0 _sinOsc_SF

-- | `sinOsc` with phase modulation as a third input node.
sinOscPM :: Node e Ampl -> Node e Freq -> Node e Phase -> Node e SynthVal
sinOscPM = mkNode3 _osc_s0 _sinOscPM_SF

-- | `sinOscPM` with feedback
sinOscFbkPM :: Node e Ampl -> Node e Freq -> Node e Phase -> Node e Ampl -> Node e SynthVal
sinOscFbkPM = mkNode4 _osc_fbk_s0 _sinOscFbkPM_SF

---------- square oscillator with adjustable duty cycle
_sqrOsc_fn :: TimeVal -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sqrOsc_fn d a f duty s = _osc_fn d f s $ \x -> if x <= duty then (-a) else a

_sqrOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_sqrOsc_SF (MkSigEnv { d = d }) a f = _sqrOsc_fn d a f 0.5

_sqrOscDM_SF :: SigEnv e -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sqrOscDM_SF (MkSigEnv { d = d }) a f duty = _sqrOsc_fn d a f duty

-- | Square oscillator taking amplitude and frequency as input nodes.
sqrOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sqrOsc = MkNode2 _osc_s0 _sqrOsc_SF

-- | `sqrOsc` with duty cycle as a third input node.
sqrOscDM :: Node e Ampl -> Node e Freq -> Node e Percent -> Node e SynthVal
sqrOscDM = mkNode3 _osc_s0 _sqrOscDM_SF

---------- saw oscillator with adjustable skew (0.5 = triangle, 0.0 or 1.0 = saw)
_saw_fn :: Ampl -> Percent -> Percent -> SynthVal
_saw_fn a skew x = let y = if x < skew then (x / skew) else (1 - x) / (1 - skew)
  in a * (2 * y - 1)

_sawOsc_fn :: TimeVal -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sawOsc_fn d a f skew s = _osc_fn d f s $ _saw_fn a skew

_sawOscSM_SF :: SigEnv e -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sawOscSM_SF (MkSigEnv { d = d }) a f = _sawOsc_fn d a f

_sawOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_sawOsc_SF (MkSigEnv { d = d }) a f = _sawOsc_fn d a f 1.0

_triOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_triOsc_SF (MkSigEnv { d = d }) a f = _sawOsc_fn d a f 0.5

-- | Saw oscillator taking amplitude and frequency as input nodes.
sawOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sawOsc = MkNode2 _osc_s0 _sawOsc_SF

-- | Triangle oscillator taking amplitude and frequency as input nodes.
triOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
triOsc = MkNode2 _osc_s0 _triOsc_SF

-- | `sawOsc` with skew as a third input node. (0 and 1 = saw, 0.5 = triangle)
sawOscSM :: Node e Ampl -> Node e Freq -> Node e Percent -> Node e SynthVal
sawOscSM = mkNode3 _osc_s0 _sawOscSM_SF

-- | `triOsc` with \"phase modulation\", almost like a \"coarser\", tri-shaped `sinOscPM`.
triOscPM :: Node e Ampl -> Node e Freq -> Node e Phase -> Node e SynthVal
triOscPM = let r2pi = recip twopi in mkNode3 _osc_s0 $ \(MkSigEnv { d = d }) a f phase s ->
  _osc_fn d f s $ \x -> _saw_fn a 0.5 (x + phase * r2pi)
