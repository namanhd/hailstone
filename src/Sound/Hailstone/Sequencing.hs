{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sound.Hailstone.Sequencing
( Cell(..)
, Now(..)
, EnvelopeCellDurationMode(..), RetriggerMode(..)
, retriggerWith
)
where

import Data.List (sortOn)
import Data.Functor ((<&>))
import Sound.Hailstone.Synth

-- | A `Now` is what an instrument receives from a `Node` to play a `Cell`.
--
-- Varying frequency or amplitude (per-note portamento, vibrato, or volume slides) specified
-- by `Cell`'s `MkC` (non-articulated) or `MkAC` (articulated) constructors are reflected as
-- time-varying `Now` sent to the instrument upon every new sample. We can add more metadata
-- here, such as modulation knobs that can be mapped to any parameter in the instrument.
data Now = MkNow
  { freq :: !Freq -- ^ current frequency
  , ampl :: !Ampl -- ^ current amplitude
  , pan :: !Pan -- ^ current pan
  , env :: !SynthVal -- ^ current value of the envelope
  } deriving (Show, Eq)

data Cell e =
  MkC -- ^ non-articulated cell
    { freq :: !Freq -- ^Frequency of the note
    , ampl :: !Ampl -- ^Amplitude of the note as a linear multiplier (should be between 0 and 1)
    , start :: !TimeVal -- ^Start time of the note in seconds.
    , dur :: !TimeVal
    -- ^Duration of the note in seconds, which can differ from `adsrTotalTime`, but whether it
    -- or the ADSR total time gets used as the time at which the note is cut is configured via
    -- an argument to `retriggerWith`.
    , pan :: !Pan
    -- ^Pan of a note, 0 is hard left, 0.5 is center, 1 is hard right.
    , adsr :: !ADSRParams
    -- ^ The envelope settings for this note.
    }
  | MkAC
    { freqs :: !(Node e Freq)
    , ampls :: !(Node e Ampl)
    , start :: !TimeVal
    , dur :: !TimeVal
    , pans :: !(Node e Pan)
    , env :: !(Node e Percent)
    }

-- | Render a `Cell` into a `Node` of `Now`. This is where we'll do \"effect commands\"
-- on cells by rendering them into a `Node` of time-varying freq/ampl/env/pan parameters.
renderCell :: EnvelopeCellDurationMode -> Cell e -> (Node e Now, TimeVal, TimeVal)
renderCell envCellDurMode (MkC freq ampl sta du pan adsrSpec) = (nowNode, sta, du')
  where
    nowNode = adsr' adsrSpec <&> \env -> MkNow {..}
    du' = case envCellDurMode of
      EnvelopeIgnoresCellDuration -> max (adsrTotalTime adsrSpec) du
      EnvelopeCutsAtCellDuration -> du
renderCell _ (MkAC freqs ampls sta du pans env) = (nowNode, sta, du)
  where
    nowNode = mkNode4_ (\_ -> MkNow) freqs ampls pans env

-- | assumed to start at 0
calcDur :: [(a, TimeVal, TimeVal)] -> TimeVal
calcDur = go 0
  where
    go latestEnd [] = latestEnd
    go latestEnd ((_, sta, du):rest) = let end = sta + du in
      if end > latestEnd then go end rest else go latestEnd rest

-- | A setting for `retriggerWith` determining whether to cut the cell's envelope when we're
-- at its duration (which is `EnvelopeCutsAtCellDuration`) or let the envelope finish.
data EnvelopeCellDurationMode = EnvelopeIgnoresCellDuration | EnvelopeCutsAtCellDuration
  deriving (Eq, Show)

-- | A setting for `retriggerWith` determining if the cells will be monophonic or polyphonic
-- (i.e. cells can play over each other) as cells do specify their start time & can overlap.
-- If `RetrigMonophonic`, we'll sort the cell list by start time and queue them back to back
-- ignoring all cell durations except for the last one.  If `RetrigPolyphonic`, we'll simply
-- schedule all cells to start at their specified start (and possibly end) time, and summing
-- up the resulting nodes (letting them play over each other freely.)
data RetriggerMode = RetrigMonophonic | RetrigPolyphonic
  deriving (Eq, Show)

-- | Causes non-positive amplitude in a `Now` to fully bypass an instrument.
--
-- Useful for when a `Now` needs to indicate a rest, as without this, the instrument is
-- going to waste time on rendering yet its result will just be all 0 due to zero amplitude.
guardByLCAmpl :: Num a => Node e a -> Node e Now -> Node e a
{-# SPECIALIZE guardByLCAmpl :: Node e (LR SynthVal) -> Node e Now -> Node e (LR SynthVal) #-}
guardByLCAmpl node = mkNode1IO node $ \r lc myNode ->
  if lc.ampl <= 0 then pure (MkS2 0 myNode) else runNode r myNode

-- | Play lists of `Cell`s with synths (a node parameterized by nodes of `Now` values).
-- \"Resets and retriggers\" the instrument in sync with note data.
retriggerWith :: EnvelopeCellDurationMode
              -> RetriggerMode -- ^monophonic or polyphonic triggering
              -> TimeVal -- ^a start time for this retriggering sequence
              -> LR SynthVal -- ^empty value for when notes have concluded
              -> [([Cell e], (Node e Now -> Node e (LR SynthVal)))]
              -- ^list of parts, each one a (cell list, instrument for this part). 
              -- An instrument is parameterized by a node of `Now`s.
              -> Node e (LR SynthVal)
retriggerWith envCellDurMode retrigMode t0 empt cellsInstrs = out
  where
    ~nodesDurs = flip concatMap cellsInstrs $ \(cells, instrument) -> cells <&> \cell -> let
      (lc', sta, du) = renderCell envCellDurMode cell
      lc = share lc'
      node = share $ guardByLCAmpl (instrument lc) lc
      in (node, sta, du)
    out = case retrigMode of
      RetrigMonophonic -> piecewiseMono t0 empt $ sortOn (\(_, sta, _) -> sta) nodesDurs
      RetrigPolyphonic -> share $ piecewisePoly t0 empt nodesDurs

