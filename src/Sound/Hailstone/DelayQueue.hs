{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module Sound.Hailstone.DelayQueue
(DelayQueue, pop, push, dummy, unDummy)
where

import qualified Data.Array.IO as Array
import Data.IORef
import Sound.Hailstone.Types (SynthVal, LR)

-- | specialized to unboxed types... maybe we could also write this as a data family so that
-- it can choose between "Data.Array.IO" and "Data.Array.Storable" depending on if the thing
-- is just a Storable or also unboxed.
data DelayQueue a = MkDQ
  { _arr :: ~(Array.IOUArray Int a)
  , _riRef :: ~(IORef Int)
  , _wiRef :: ~(IORef Int)
  , _len :: !Int
  }

-- | Basically used as a @Nothing :: (Maybe (DelayQueue a))@ unwrapped by `unDummy`.
dummy :: DelayQueue a
dummy = MkDQ undefined undefined undefined (-1)

unDummy :: (Array.MArray Array.IOUArray a IO) => Int -> a -> DelayQueue a -> IO (DelayQueue a)
{-# SPECIALIZE unDummy :: Int -> LR SynthVal -> DelayQueue (LR SynthVal) -> IO (DelayQueue (LR SynthVal)) #-}
unDummy n empt dq@(MkDQ _ _ _ len)
  | len < 0 && n > 0 = do
      riRef <- newIORef 1
      wiRef <- newIORef 0
      arr <- Array.newArray (0, n - 1) empt
      pure $ MkDQ arr riRef wiRef n
  | len < 0 && n <= 0 = error "can't init nonpositive"
  | otherwise = pure dq

-- | push an element to the queue
push :: (Array.MArray Array.IOUArray a IO) => a -> DelayQueue a -> IO ()
{-# SPECIALIZE push :: LR SynthVal -> DelayQueue (LR SynthVal) -> IO () #-}
push a (MkDQ arr riRef wiRef len) = do
  ri <- readIORef riRef
  wi <- readIORef wiRef
  let wiNew = (wi + 1) `mod` len
  if wiNew == ri then pure () else do
    writeIORef wiRef wiNew
    Array.writeArray arr wi a

-- | pop an element from the queue
pop :: (Array.MArray Array.IOUArray a IO) => a -> DelayQueue a -> IO a
{-# SPECIALIZE pop :: LR SynthVal -> DelayQueue (LR SynthVal) -> IO (LR SynthVal) #-}
pop empt (MkDQ arr riRef wiRef len) = do
  ri <- readIORef riRef
  wi <- readIORef wiRef
  if ri == wi then pure empt else do
    writeIORef riRef $ (ri + 1) `mod` len
    Array.readArray arr ri
