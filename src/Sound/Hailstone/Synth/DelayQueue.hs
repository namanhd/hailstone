{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sound.Hailstone.Synth.DelayQueue
(initDQ, withDelay)
where

import qualified Data.Array.IO as IOA
import Sound.Hailstone.Synth.SynthVal
import Sound.Hailstone.Synth.LR
import Sound.Hailstone.Synth.MiscTypes (SPair(..))

-- | specialized to unboxed types... maybe we could also write this as a data family so that
-- it can choose between "Data.IOA.IO" and "Data.IOA.Storable" depending on if the thing
-- is just a Storable or also unboxed.
data DQ a = MkDQ
  { _arr :: ~(IOA.IOUArray Int a)
  , _ri :: !Int
  , _wi :: !Int
  , _len :: !Int
  }

initDQ :: (IOA.MArray IOA.IOUArray a IO) => TimeVal -> TimeVal -> a -> IO (DQ a)
{-# SPECIALIZE initDQ :: TimeVal -> TimeVal -> LR SynthVal -> IO (DQ (LR SynthVal)) #-}
initDQ delaySec d empt = do
  let n = round $ delaySec / d
  arr <- IOA.newArray (0, n - 1) empt
  pure $ MkDQ arr 1 0 n

-- | push an element to the queue
push :: (IOA.MArray IOA.IOUArray a IO) => a -> DQ a -> IO (DQ a)
{-# SPECIALIZE push :: LR SynthVal -> DQ (LR SynthVal) -> IO (DQ (LR SynthVal)) #-}
push a dq@(MkDQ arr ri wi len) = do
  let wiNew = (wi + 1) `mod` len
  if wiNew == ri then pure dq else do
    IOA.writeArray arr wi a
    pure $ dq { _wi = wiNew }

-- | pop an element from the queue
pop :: (IOA.MArray IOA.IOUArray a IO) => a -> DQ a -> IO (SPair a (DQ a))
{-# SPECIALIZE pop :: LR SynthVal -> DQ (LR SynthVal) -> IO (SPair (LR SynthVal) (DQ (LR SynthVal))) #-}
pop empt dq@(MkDQ arr ri wi len) = do
  if ri == wi then pure (MkS2 empt dq) else do
    a <- IOA.readArray arr ri
    pure $ MkS2 a (dq { _ri = (ri + 1) `mod` len })

withDelay :: Ampl -> DQ (LR SynthVal) -> LR SynthVal -> (DQ (LR SynthVal) -> LR SynthVal -> LR SynthVal -> IO b) -> IO b
withDelay decayMult dq x k = do
  (MkS2 delayOut dq') <- pop zeroLR dq
  let delayIn = x + (decayMult .*: delayOut)
  dq'' <- push delayIn dq'
  k dq'' delayOut delayIn
{-# INLINE withDelay #-}
