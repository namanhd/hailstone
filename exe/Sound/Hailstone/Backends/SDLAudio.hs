{-# LANGUAGE GADTs, BangPatterns #-}

module Sound.Hailstone.Backends.SDLAudio
( openAudio
, enableAudio
, putNode
, lockAudio
, resumeAudio
, closeAudio
)
where

import Control.Concurrent.MVar

-- for SDL
import qualified SDL as SDL
import Data.Word (Word16) -- for buffer size
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Vector.Unboxed as VU

import Sound.Hailstone.Synth

timeLookahead :: ChanMode -> TimeVal -> Int -> TimeVal -> TimeVal
timeLookahead !cm !t !n !d = if cm == Mono then t + (fromIntegral n) * d
  else t + (fromIntegral $ n `div` 2) * d

consumeSink :: Int -> Sink -> SMV.IOVector SampleVal -> IO Sink
consumeSink !buffLen !sink !audioBuf = ioNewSink
  where
    d = getDeltaTime sink       -- delta time (= 1/sample rate)
    n0 = getDestNode sink       -- audio node
    t0 = getCurrTime sink       -- start time for this buffer
    cm = getChanMode sink       -- channel mode of sink

    f :: Node (LR SampleVal) -> Int -> IO (Node (LR SampleVal))
    f !node !idx = (case cm of
        Stereo -> SMV.write audioBuf idx l *> SMV.write audioBuf (idx + 1) r
        Mono -> SMV.write audioBuf idx (l + r)) *> pure newNode
      where
        !t = timeLookahead cm t0 idx d
        (MkLR (!l, !r), newNode) = runNode (t, d) node
    {-# INLINE f #-}

    -- if stereo mode, then the index goes up in increments of 2 until it hits buffLen.
    -- timeLookahead should already handle these indices properly given chanMode
    -- (because stereo indices are even = left, odd = right), and the node generates
    -- samples in LR pairs, so we only need to sample the node on even indices.
    indices = if cm == Mono then VU.generate buffLen id else VU.generate (buffLen `div` 2) (* 2)
    ioNewSink = (\(!finalNode) -> sink
      { getCurrTime = timeLookahead cm t0 buffLen d
      , getDestNode = finalNode
      }) <$> VU.foldM f n0 indices

-- | SDL audio callback function; called whenever SDL needs more data in the
-- audio buffer. This should copy samples into the buffer, or pad with silence
-- if there is no more data to play
sdlAudioCallback :: MVar Sink  -- MVar state
                 -> SDL.AudioFormat s  -- SDL Audio Format identifier
                 -> SMV.IOVector s     -- Mutable IO vector containing samples
                 -> IO ()
sdlAudioCallback mSink sdlAudioFormat sdlAudioBuffer = case sdlAudioFormat of
  SDL.Signed16BitLEAudio -> do   -- turns out we need GADTs turned on for this
    !sink <- takeMVar mSink
    let buffLen = SMV.length sdlAudioBuffer
    -- then copy the bytes into the IO vector, and consume the state throughout
    modifiedSink <- consumeSink buffLen sink sdlAudioBuffer

    -- update the signal state MVar with the remainder of the signal
    putMVar mSink modifiedSink
  _ -> putStrLn "Unsupported audio sample format"


-- | Initialize SDL (the Audio subsystem only) given a sample rate and buffer
-- size, and create an `MVar` to store the output `SampleHose` signal.
openAudio :: SampleRate -> Word16 -> ChanMode -> IO (SDL.AudioDevice, MVar Sink)
openAudio sampleRate bufferSize chanMode = do
  SDL.initialize [SDL.InitAudio]
  let sampleType = SDL.Signed16BitNativeAudio
      stereoMode = case chanMode of
        Mono -> SDL.Mono
        Stereo -> SDL.Stereo

  mSink <- newMVar $ initSink sampleRate chanMode
  -- function "requests" an audio spec from the hardware, using the
  -- requests/demands we specify in the Spec. It returns a spec, that has the
  -- true values that were provided, which hopefully matches our demands but
  -- might not exactly match our demands. (the spec is the 2nd tuple item, but
  -- that is unused)
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate $ fromIntegral sampleRate
    , SDL.openDeviceFormat = SDL.Mandate sampleType
    , SDL.openDeviceChannels = SDL.Mandate stereoMode
    , SDL.openDeviceSamples = bufferSize
    , SDL.openDeviceCallback = sdlAudioCallback mSink
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing -- any output audio device will do
    }
  -- TODO we should grab (SDL.audioSpecSilence spec) and somehow tell the Synth
  -- module that that value is what represents silence (for now we default to 0)
  pure (device, mSink)

-- | Closes SDL audio. Should be called at the end of main.
closeAudio :: SDL.AudioDevice -> IO ()
closeAudio = SDL.closeAudioDevice

-- | Modify the MVar to store a new audio node tree.
putNode :: MVar Sink -> Node (LR SampleVal) -> IO ()
putNode mSink newNode = do
  currSink <- takeMVar mSink
  -- update the sample source and reset the time counter, but don't change
  -- anything about the delta value and the curr-chan
  let newSink = currSink { getDestNode = newNode, getCurrTime = 0.0 }
  putMVar mSink newSink

-- | Enable SDL audio. Should be called at the start of main.
enableAudio :: SDL.AudioDevice -> IO ()
enableAudio dev = SDL.setAudioDevicePlaybackState dev SDL.Play

-- | Pauses SDL audio, preventing the callback from firing I think.
lockAudio :: SDL.AudioDevice -> IO ()
lockAudio dev = SDL.setAudioDeviceLocked dev SDL.Locked

-- | Resumes SDL audio.
resumeAudio :: SDL.AudioDevice -> IO ()
resumeAudio dev = SDL.setAudioDeviceLocked dev SDL.Unlocked
