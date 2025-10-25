{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Sound.Hailstone.Backends.PortAudio
( HailstoneAudioHandle
, withAudio
, enableAudio
, putNode
, lockAudio
, resumeAudio
)
where

-- PortAudio backend specific
import qualified Sound.PortAudio as PA
import qualified Foreign.Storable (pokeElemOff)

import System.Mem (performGC)
import Data.Word (Word16)
import Control.Concurrent
import qualified Sound.Hailstone.Backends.Common as Common
import Sound.Hailstone.Synth

data HailstoneAudioHandle e = forall format. (PA.StreamFormat format) => MkHAH
  { _HAHpaStream :: PA.Stream format format
  -- ^ PortAudio stream
  , _HAHchanMode :: ChanMode
  -- ^ mono or stereo
  , _HAHsampleRate :: SampleRate
  -- ^ sample rate
  , _HAHreplacementSinkMV :: MVar (Sink e)
  -- ^ comms channel to give the producer thread a new sink to play
  }

-- | callback that PortAudio will fire to fill a buffer of samples to play
paAudioCallback :: ChanMode -- ^ stereo or mono
                -> Common.SampleQueue
                -> PA.StreamCallback SampleVal SampleVal
paAudioCallback cm sampQ _ _ nSamplesPerBuffer _ outPtr = let
  realBufLen = (fromIntegral nSamplesPerBuffer) * (case cm of Stereo -> 2; _ -> 1)
  writer = Foreign.Storable.pokeElemOff outPtr
  in Common.consumerLoop realBufLen cm sampQ PA.Continue writer

-- | Run PortAudio given a sample rate, buffer size, channel mode, extra signal environment,
-- initial node graph, and an IO action, and then terminate.
withAudio :: SampleRate  -- ^sample rate
          -> Word16 -- ^number of samples per buffer
          -> ChanMode -- ^stereo or mono
          -> e -- ^extra signal environment
          -> Node e (LR SampleVal) -- ^node graph
          -> (HailstoneAudioHandle e -> IO ()) -- ^IO actions given the handle (e.g. `enableAudio`)
          -> IO ()
withAudio sampleRate nSamplesPerBuffer chanMode initEnv initNode action = (*> pure ()) . PA.withPortAudio $ do
  let fresh = initSink sampleRate initEnv
      sink = fresh { _destNode = initNode }

  -- start the producer thread
  (sampQ, replacementSinkMV, producerThreadId) <-
    Common.startProducer nSamplesPerBuffer chanMode sink

  -- maybe this helps to reduce initial stutters by preemptively GC'ing
  performGC

  PA.withDefaultStream 0 (case chanMode of Stereo -> 2; _ -> 1)
    (fromIntegral sampleRate)
    (Just $ fromIntegral nSamplesPerBuffer)
    (Just $ paAudioCallback chanMode sampQ)
    (Just $ pure ()) $ \stream -> do
      action $ MkHAH
        { _HAHpaStream = stream
        , _HAHchanMode = chanMode
        , _HAHsampleRate = sampleRate
        , _HAHreplacementSinkMV = replacementSinkMV
        }
      _ <- PA.stopStream stream
      killThread producerThreadId
      pure $ Right ()

-- | Play a new node graph.
putNode :: e -> Node e (LR SampleVal) -> HailstoneAudioHandle e -> IO ()
putNode newEnv newNode hah = do
  -- update the sample source (and reset the time counter), but don't change
  -- anything about the delta value
  let fresh = initSink (_HAHsampleRate hah) newEnv
      newSink = fresh { _destNode = newNode }
      replacementSinkMV = _HAHreplacementSinkMV hah
  putMVar replacementSinkMV newSink
  pure ()

-- | Enable the PortAudio stream, beginning playback.
enableAudio :: HailstoneAudioHandle e -> IO ()
enableAudio (MkHAH {_HAHpaStream = stream}) = PA.startStream stream *> pure ()

-- | Pauses the PortAudio stream. (This prevents the PortAudio callback from firing.)
lockAudio :: HailstoneAudioHandle e -> IO ()
lockAudio (MkHAH {_HAHpaStream = stream}) = PA.stopStream stream *> pure ()

-- | Resumes the PortAudio stream. (For PortAudio, this is an alias of `enableAudio`, here
-- only for SDL interface compatibility.)
resumeAudio :: HailstoneAudioHandle e -> IO ()
resumeAudio = enableAudio
