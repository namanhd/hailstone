{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sound.Hailstone.DelayQueue
(delay_s0, withDelay)
where

import qualified Data.Array.IO as IOA
import Sound.Hailstone.Types

-- | specialized to unboxed types... maybe we could also write this as a data family so that
-- it can choose between "Data.IOA.IO" and "Data.IOA.Storable" depending on if the thing
-- is just a Storable or also unboxed.
data DQ a = MkDQ
  { _arr :: ~(IOA.IOUArray Int a)
  , _ri :: !Int
  , _wi :: !Int
  , _len :: !Int
  }

-- | Initial state for a delay node. Defers allocating the array to the first run, such that
-- we don't need IO just to init a node. Treated as a @Nothing :: (Maybe (DQ a))@, unwrapped
-- by @unDummy@.
delay_s0 :: DQ a
delay_s0 = MkDQ undefined 1 0 (-1)

unDummy :: (IOA.MArray IOA.IOUArray a IO) => TimeVal -> TimeVal -> a -> DQ a -> IO (DQ a)
{-# SPECIALIZE unDummy :: TimeVal -> TimeVal -> LR SynthVal -> DQ (LR SynthVal) -> IO (DQ (LR SynthVal)) #-}
unDummy delaySec d empt dq@(MkDQ _ _ _ len)
  | len < 0 = do
      let n = round $ delaySec / d
      arr <- IOA.newArray (0, n - 1) empt
      pure $ MkDQ arr 1 0 n
  | otherwise = pure dq

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

-- | perform the delay pop-push then run a function on the resulting queue, the delay output
-- and the next delay input (which is mixed with the source signal with a decay factor.)
withDelay :: TimeVal -> Ampl -> TimeVal -> DQ (LR SynthVal) -> LR SynthVal -> (DQ (LR SynthVal) -> LR SynthVal -> LR SynthVal -> IO b) -> IO b
withDelay delaySec decayMult d savedDQ x k = do
  dq <- unDummy delaySec d zeroLR savedDQ
  (MkS2 delayOut dq') <- pop zeroLR dq
  let delayIn = x + (decayMult .*: delayOut)
  dq'' <- push delayIn dq'
  k dq'' delayOut delayIn
{-# INLINE withDelay #-}
