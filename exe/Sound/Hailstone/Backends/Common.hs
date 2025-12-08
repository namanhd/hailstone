{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Common code for real-time (callback-firing) backends (PortAudio, SDL audio): a producer
-- thread that fills an (unboxed) concurrent channel with samples, and a consumer loop to be
-- used by callbacks to read samples from the channel and send/write to the audio device.
module Sound.Hailstone.Backends.Common
( startProducer
, consumerLoop
, SampleQueue
)
where

import Data.Word (Word16)
import Control.Concurrent
import GHC.Conc
import GHC.Exts (tryTakeMVar#)
import qualified GHC.Base
import qualified GHC.MVar

import Sound.Hailstone.Synth.Node
import qualified Data.Array.IO as Array

-- | single-producer single-consumer bounded unboxed concurrent queue / ring buffer
data SPSCQueue a = MkSPSC
  { _arr :: Array.IOUArray Int a
  , _riRef :: TVar Int
  , _wiRef :: TVar Int
  , _len :: !Int
  }

initSPSC :: (Array.MArray Array.IOUArray a IO) => Int -> a -> IO (SPSCQueue a)
{-# SPECIALIZE initSPSC :: Int -> SampleVal -> IO (SPSCQueue SampleVal) #-}
initSPSC n empt
  | n > 0 = do
      riRef <- newTVarIO 0
      wiRef <- newTVarIO 0
      arr <- Array.newArray (0, n - 1) empt
      pure $ MkSPSC arr riRef wiRef n
  | otherwise = error "can't init nonpositive"

pushSPSC :: (Array.MArray Array.IOUArray a IO) => a -> SPSCQueue a -> IO ()
{-# SPECIALIZE pushSPSC :: SampleVal -> SPSCQueue SampleVal -> IO () #-}
pushSPSC a (MkSPSC arr riRef wiRef len) = do
  wi <- readTVarIO wiRef
  let wiNew = (wi + 1) `mod` len
  atomically $ do
    ri <- readTVar riRef
    if wiNew == ri then retry else writeTVar wiRef wiNew
  -- if we're here, that means the wi updated and we're unblocked, can go ahead and write
  Array.writeArray arr wi a

popSPSC :: (Array.MArray Array.IOUArray a IO) => SPSCQueue a -> IO a
{-# SPECIALIZE popSPSC :: SPSCQueue SampleVal -> IO SampleVal #-}
popSPSC (MkSPSC arr riRef wiRef len) = do
  ri <- readTVarIO riRef
  atomically $ do
    wi <- readTVar wiRef
    if ri == wi then retry else let riNew = (ri + 1) `mod` len in writeTVar riRef riNew
  Array.readArray arr ri

type SampleQueue = SPSCQueue SampleVal

-- | this @withDefault@ function should allocate less than `tryTakeMVar` which returns @IO
-- (Maybe a)@ rather than letting you put in a default return
tryTakeMVarWithDefault :: GHC.MVar.MVar a -> a -> IO a
tryTakeMVarWithDefault (GHC.MVar.MVar m) dflt = GHC.Base.IO $ \s -> case tryTakeMVar# m s of
  (# s', 0#, _ #) -> (# s', dflt #)
  (# s', _, !a #) -> (# s', a #)

-- | loop body for producer thread
producerLoop :: ChanMode -> SampleQueue -> MVar (Sink e) -> Sink e -> IO ()
producerLoop cm sampQ replacementSinkMV sink = do
    !(MkSink { destNode = destNode, sigEnv = sigEnv }) <- tryTakeMVarWithDefault replacementSinkMV sink
    let !node = destNode
    let !r@(MkSigEnv { t = t, d = d }) = sigEnv
    !(MkS2 x nextNode) <- runNode r node
    -- TODO we can do something with this e. like get outside info, MIDI messages, etc
    let !nextSink = MkSink { destNode = nextNode, sigEnv = r { t = t + d, d = d } }
    case cm of
      Stereo -> withLR x $ \(!xl) (!xr) -> pushSPSC xl sampQ *> pushSPSC xr sampQ
      Mono -> pushSPSC (hsumLR x `div` 4) sampQ   -- todo properly rescale this
    producerLoop cm sampQ replacementSinkMV nextSink

-- | Consumer loop, meant for callbacks to call with their own specific writing function.
consumerLoop  :: Int -- ^length of the actual buffer to write to; should be n. of frames times n. channels
              -> ChanMode -- ^stereo or mono
              -> SampleQueue -- ^sample queue
              -- -> SampleChOut -- ^output end of the sample channel the producer is writing to
              -- -> SampleChCountTVar -- ^queued count
              -> a -- ^return value to signal the callback is done
              -> (Int -> SampleVal -> IO ()) -- ^IO action that writes a sample at an index
              -> IO a
consumerLoop realBufLen cm sampQ ret writer = loop 0
  where
    writeat ii = popSPSC sampQ >>= writer ii
    loop i = if i >= realBufLen then pure ret else case cm of
      Stereo -> writeat i *> writeat (i + 1) *> loop (i + 2)
      Mono -> writeat i *> loop (i + 1)
{-# INLINE consumerLoop #-}

-- | Start the producer thread which will loop forever and write to the sample chan.
startProducer :: Word16 -- ^number of samples/frames per buffer
              -> ChanMode
              -> Sink e -- ^ `Sink` to start playing with
              -> IO (SampleQueue, MVar (Sink e), ThreadId)
startProducer nSamplesPerBuffer cm sink = do
  -- this (2*) should be configurable? if realtime response to live input needed, this ought
  -- to be low, otherwise we can just let producer run ahead of consumer for as long as we want
  let !maxQueueLen = (2 *) . fromIntegral $ nSamplesPerBuffer * (case cm of Stereo -> 2; _ -> 1)
  -- comms channel for the producer to give us samples to copy to the backend buffer
  sampQ <- initSPSC maxQueueLen 0
  -- comms channel for us to give the producer new replacement sinks to play
  replacementSinkMV <- newEmptyMVar
  -- comms channel to limit the number of samples in the queue
  threadId <- forkIO $ producerLoop cm sampQ replacementSinkMV sink
  pure (sampQ, replacementSinkMV, threadId)
