module Sound.Hailstone.DelayQueue
(DelayQueue, pop, push, dummy, unDummy)
where

import qualified Data.Array.Storable as Array
import Data.IORef
import Sound.Hailstone.Types (SynthVal, LR)
import Foreign (Storable)

data DelayQueue a = MkDQ
  { _arr :: Array.StorableArray Int a
  , _riRef :: IORef Int
  , _wiRef :: IORef Int
  , _len :: !Int
  }

dummy :: DelayQueue a
dummy = MkDQ undefined undefined undefined (-1)

unDummy :: Storable a => Int -> a -> DelayQueue a -> IO (DelayQueue a)
{-# SPECIALIZE unDummy :: Int -> LR SynthVal -> DelayQueue (LR SynthVal) -> IO (DelayQueue (LR SynthVal)) #-}
unDummy n empt dq@(MkDQ _ _ _ len)
  | len < 0 && n > 0 = do
      riRef <- newIORef 0
      wiRef <- newIORef 1
      arr <- Array.newArray (0, n - 1) empt
      pure $ MkDQ arr riRef wiRef n
  | len < 0 && n <= 0 = error "can't init nonpositive"
  | otherwise = pure dq

push :: Storable a => a -> DelayQueue a -> IO ()
{-# SPECIALIZE push :: LR SynthVal -> DelayQueue (LR SynthVal) -> IO () #-}
push a (MkDQ arr riRef wiRef len) = do
  ri <- readIORef riRef
  wi <- readIORef wiRef
  if ((wi + 1) `mod` len) == ri then pure () else do
    writeIORef wiRef $ (wi + 1) `mod` len
    Array.writeArray arr wi a

pop :: Storable a => a -> DelayQueue a -> IO a
{-# SPECIALIZE pop :: LR SynthVal -> DelayQueue (LR SynthVal) -> IO (LR SynthVal) #-}
pop empt (MkDQ arr riRef wiRef len) = do
  ri <- readIORef riRef
  wi <- readIORef wiRef
  if ri == wi then pure empt else do
    writeIORef riRef $ (ri + 1) `mod` len
    Array.readArray arr wi
