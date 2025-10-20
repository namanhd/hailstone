{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

-- | Common code for real-time (callback-firing) backends (PortAudio, SDL audio): a producer
-- thread that fills an (unboxed) concurrent channel with samples, and a consumer loop to be
-- used by callbacks to read samples from the channel and send/write to the audio device.
module Sound.Hailstone.Backends.Common
( startProducer
, consumerLoop
, SampleChOut
, SampleChCountTVar
)
where

import Data.Word (Word16)
import Control.Concurrent
import GHC.Conc
import GHC.Exts (tryTakeMVar#)
import qualified Control.Concurrent.Chan.Unagi.Unboxed as UU  -- concurrent FIFO channel
import qualified GHC.Base
import qualified GHC.MVar

import Sound.Hailstone.Synth

type SampleChIn = UU.InChan SampleVal
type SampleChOut = UU.OutChan SampleVal
type SampleChCountTVar = TVar Int

-- | this @withDefault@ function should allocate less than `tryTakeMVar` which returns @IO
-- (Maybe a)@ rather than letting you put in a default return
tryTakeMVarWithDefault :: GHC.MVar.MVar a -> a -> IO a
tryTakeMVarWithDefault (GHC.MVar.MVar m) dflt = GHC.Base.IO $ \s -> case tryTakeMVar# m s of
  (# s', 0#, _ #) -> (# s', dflt #)
  (# s', _, !a #) -> (# s', a #)

-- | loop body for producer thread
producerLoop :: Int -> ChanMode -> SampleChIn -> SampleChCountTVar -> MVar (Sink e) -> Sink e -> IO ()
producerLoop maxQueueLen cm sampChI queuedCountTV replacementSinkMV sink = do
    !sinkToPlayNow <- tryTakeMVarWithDefault replacementSinkMV sink
    let !node = _destNode sinkToPlayNow
    let !r@(t, d, e) = _sigEnv sinkToPlayNow
    !(MkS2 (MkLR xl xr) nextNode) <- runNode r node
    -- TODO we can do something with this e. like get outside info, MIDI messages, etc
    let !nextSink = MkSink { _destNode = nextNode, _sigEnv = (t + d, d, e) }
    -- write samples to the channel
    case cm of
      Stereo -> do
        atomically $ do
          qc <- readTVar queuedCountTV
          if qc > maxQueueLen then retry else let !qc' = qc + 2 in writeTVar queuedCountTV qc'
        UU.writeChan sampChI xl
        UU.writeChan sampChI xr
      Mono -> do
        atomically $ do
          qc <- readTVar queuedCountTV
          if qc > maxQueueLen then retry else let !qc' = qc + 1 in writeTVar queuedCountTV qc'
        UU.writeChan sampChI (xl + xr)
    -- loop
    producerLoop maxQueueLen cm sampChI queuedCountTV replacementSinkMV nextSink

-- | Consumer loop, meant for callbacks to call with their own specific writing function.
consumerLoop  :: Int -- ^length of the actual buffer to write to; should be n. of frames times n. channels
              -> ChanMode -- ^stereo or mono
              -> SampleChOut -- ^output end of the sample channel the producer is writing to
              -> SampleChCountTVar -- ^queued count
              -> a -- ^return value to signal the callback is done
              -> (Int -> SampleVal -> IO ()) -- ^IO action that writes a sample at an index
              -> IO a
consumerLoop realBufLen cm sampChO queuedCountTV ret writer = loop 0
  where
    writeat ii = UU.readChan sampChO >>= writer ii
    loop i = if i >= realBufLen then pure ret else case cm of
      Stereo -> do
        writeat i
        writeat (i + 1)
        atomically (readTVar queuedCountTV >>= \(!qc) -> writeTVar queuedCountTV (qc - 2))
        loop (i + 2)
      Mono -> do
        writeat i
        atomically (readTVar queuedCountTV >>= \(!qc) -> writeTVar queuedCountTV (qc - 1))
        loop (i + 1)
{-# INLINE consumerLoop #-}

-- | Start the producer thread which will loop forever and write to the sample chan.
startProducer :: Word16 -- ^number of samples/frames per buffer
              -> ChanMode -- ^stereo or mono
              -> Sink e -- ^ `Sink` to start playing with
              -> IO (SampleChOut, SampleChCountTVar, MVar (Sink e), ThreadId)
startProducer nSamplesPerBuffer cm sink = do
  -- comms channel for the producer to give us samples to copy to the backend buffer
  (sampChI, sampChO) <- UU.newChan
  -- comms channel for us to give the producer new replacement sinks to play
  replacementSinkMV <- newEmptyMVar
  -- comms channel to limit the number of samples in the queue
  queuedCountTV <- newTVarIO 0
  -- make the producer thread. make this configurable? the queue should be as
  -- big as the specified buffer size... we could add some leeway. this can be as big as we
  -- want for just song playback, but it should be small (still >=bufferSize) to respond to
  -- real time inputs if we ever do that
  let !maxQueueLen = (2 *) . fromIntegral $
        nSamplesPerBuffer * (case cm of Stereo -> 2; _ -> 1)
  threadId <- forkIO $ producerLoop maxQueueLen cm sampChI queuedCountTV replacementSinkMV sink
  pure (sampChO, queuedCountTV, replacementSinkMV, threadId)
