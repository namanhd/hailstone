{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Sound.Hailstone.Sequencing
( Cell(..)
, Now(..)
, EnvelopeCellDurationMode(..), RetriggerMode(..)
, retriggerWith
)
where

import Data.List (sortOn)
import Data.Functor ((<&>))
import Sound.Hailstone.Synth.Node
import Sound.Hailstone.Sequencing.Cell
import Sound.Hailstone.Sequencing.Now

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
-- calcDur :: [(a, TimeVal, TimeVal)] -> TimeVal
-- calcDur = go 0
--   where
--     go latestEnd [] = latestEnd
--     go latestEnd ((_, sta, du):rest) = let end = sta + du in
--       if end > latestEnd then go end rest else go latestEnd rest

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
guardByNowAmpl :: Num a => Node e Now -> Node e a -> Node e a
{-# SPECIALIZE guardByNowAmpl :: Node e Now -> Node e (LR SynthVal) -> Node e (LR SynthVal) #-}
guardByNowAmpl nowNode node = mkNode1IO node (\r lc myNode ->
  if lc.ampl <= 0 then pure (MkS2 0 myNode) else runNode r myNode) nowNode

-- | Play lists of `Cell`s with synths (a node parameterized by nodes of `Now` values).
-- \"Resets and retriggers\" the instrument in sync with note data.
retriggerWith :: (MonadHasNodeSharing m)
              => EnvelopeCellDurationMode
              -> RetriggerMode -- ^monophonic or polyphonic triggering
              -> TimeVal -- ^a start time for this retriggering sequence
              -> LR SynthVal -- ^empty value for when notes have concluded
              -> [([Cell e], (Node e Now -> m (Node e (LR SynthVal))))]
              -- ^list of parts, each one a (cell list, instrument for this part).
              -- An instrument is parameterized by a node of `Now`s.
              -> m (Node e (LR SynthVal))
retriggerWith envCellDurMode retrigMode t0 empt cellsInstrs = out
  where
    ~mNodesDurs = sequenceA $ flip concatMap cellsInstrs $
      \(cells, instrument) -> cells <&> \cell -> do
        let (lc', sta, du) = renderCell envCellDurMode cell
        lc <- share lc'
        (\node -> (node, sta, du)) <$> (share =<< guardByNowAmpl lc <$> instrument lc)
    out = case retrigMode of
      RetrigMonophonic -> piecewiseMono t0 empt <$> sortOn (\(_, sta, _) -> sta) <$> mNodesDurs
      RetrigPolyphonic -> share =<< piecewisePoly t0 empt <$> mNodesDurs
