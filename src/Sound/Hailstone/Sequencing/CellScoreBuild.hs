{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sound.Hailstone.Sequencing.CellScoreBuild
( CellScoreBuild
  -- ** commands
, add, set, with, with', block, block', use, use', visit, visit', here, rep
  -- ** instrument commands
, withInstr, setInstr
  -- ** setter prefixes
, go, at, priv, andThen
  -- ** setters
, freq, itvl, ampA, ampX, gain, jump, step, durA, durX, panA, panX, envl, susX, cell
  -- ** compilation
, realizeCellScore
)
where

import qualified Data.Text as T
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Sound.Hailstone.Sequencing.Cell
import Sound.Hailstone.Sequencing.Now (Now)
import Sound.Hailstone.Synth.Node (adsr', Node, (*|), LR)
import Sound.Hailstone.Synth.Effects (gain2ampl)
import Sound.Hailstone.Synth.SynthVal
import Sound.Hailstone.Synth.MiscTypes (ADSRParams(..), SPair(..), STriple(..))

type InstrumentSymbol = T.Text

type CellScore e = M.Map InstrumentSymbol [Cell e]

--- TODO add to the state a "max instrument id" and some actionsin this monad to register a new instrument
type CellScoreBuild e a = State (STriple (Cell e) InstrumentSymbol (CellScore e)) a

_csbPutAndMakeState :: Cell e -> Cell e -> InstrumentSymbol -> CellScore e -> STriple (Cell e) InstrumentSymbol (CellScore e)
_csbPutAndMakeState addCell putCell inst score = MkS3 putCell inst $
  M.insert inst (addCell : (maybe [] id $ M.lookup inst score)) score

-- | add a cell (both add it to the score and return it) and put a cell in the state
_csbAddRetPut :: Cell e -> Cell e -> Cell e -> CellScoreBuild e (Cell e)
_csbAddRetPut addCell retCell putCell = (retCell <$) $ modify $ \(MkS3 _ inst score) -> _csbPutAndMakeState addCell putCell inst score

-- | don't add anything to the score, just put a cell in the state and return it
_csbRetPutThisOnly :: Cell e -> CellScoreBuild e (Cell e)
_csbRetPutThisOnly putCell = (putCell <$) $ modify $ \(MkS3 _ inst score) -> MkS3 putCell inst score

data CellSetterActMode
  = At  -- ^ apply the setter to the cell being added-and-returned only, not the current cell in the state
  | Priv -- ^ apply the setter to the cell being added only, NOT the one being returned (and of course also not the cell in the state)
  | Go  -- ^ apply the setter to the cell being added-and-returned AND the current cell in the state
  | Then -- ^ don't apply the setter to the cell being added-and-returned; add it, then apply the setter to the current cell in the state

-- | a function that modifies a cell
type CellMod e = Cell e -> Cell e

-- | a setter takes an argument and returns a list of edits to be applied sequentially. This
-- list is usually just a singleton for most of the setter we provide; they will be composed
-- after they've been decorated with the `CellSetterActMode` in the form of `CellAct`s.
type CellSetter e a = a -> [CellMod e]

-- | a cell-modifying function paired with how to apply it on the state-put and added cells
type CellAct e = SPair CellSetterActMode (CellMod e)

--------------------------------------------------------------------------------
-- ** commands

-- | Construct a score with a given starting instrument; afterwards return to the instrument
-- in the state from before this command.
withInstr :: InstrumentSymbol -> CellScoreBuild e (Cell e) -> CellScoreBuild e (Cell e)
withInstr inst csb = state $ \(MkS3 stateCell origInst score) -> let
  (ret, (MkS3 stateCell' _ score')) = runState csb (MkS3 stateCell inst score)
  in (ret, MkS3 stateCell' origInst score')

-- | Switch to a new current instrument.
setInstr :: InstrumentSymbol -> CellScoreBuild e (Cell e)
setInstr inst = state $ \(MkS3 stateCell _ score) -> (stateCell, MkS3 stateCell inst score)

-- | processes lists of acts given the cell-to-add, cell-to-return, and cell-to-put-in-the-state
_gogo :: Cell e -> Cell e -> Cell e -> [CellAct e] -> (Cell e -> Cell e -> Cell e -> x) -> x
_gogo addCell retCell putCell acts k = case acts of
  [] -> k addCell retCell putCell
  ((MkS2 mode f):as) -> case mode of
    Priv -> _gogo (f addCell) retCell     putCell     as k
    At ->   _gogo (f addCell) (f retCell) putCell     as k
    Go ->   _gogo (f addCell) (f retCell) (f putCell) as k
    Then -> _gogo addCell     retCell     (f putCell) as k

-- | Return the state cell at this point.
here :: CellScoreBuild e (Cell e)
here = get >>= \(MkS3 stateCell _ _) -> pure stateCell

-- | Add and return the state cell after doing some actions to it.
add :: [CellAct e] -> CellScoreBuild e (Cell e)
add acts' = get >>= \(MkS3 stateCell _ _) -> _gogo stateCell stateCell stateCell acts' _csbAddRetPut

-- | Set properties of the state cell with some setters.
set :: [CellMod e] -> CellScoreBuild e (Cell e)
set fs = get >>= \(MkS3 stateCell _ _) -> _gogo stateCell stateCell stateCell
  (map (MkS2 Go) fs) (\_ _ -> _csbRetPutThisOnly)

with_ :: Bool -> [CellMod e] -> CellScoreBuild e (Cell e) -> CellScoreBuild e (Cell e)
with_ keepNewTime fs csb = state $ \(MkS3 stateCell inst score) -> let
  putCell' = _gogo stateCell stateCell stateCell (map (MkS2 Go) fs) (\_ _ pc -> pc)
  (ret, MkS3 stateCell' _ score') = runState csb (MkS3 putCell' inst score)
  stateCell'' = if keepNewTime then stateCell { start = stateCell'.start } else stateCell
  in (ret, MkS3 stateCell'' inst score')

-- | Use a given cell with some actions applied as the starting cell state to run a score of
-- commands, then restore the last state from before this command afterwards (except for the
-- start time, which is the resulting time from the block.)
with :: [CellMod e] -> CellScoreBuild e (Cell e) -> CellScoreBuild e (Cell e)
with = with_ True

-- | Same as `with` but /also/ restoring the start time from before this command.
with' :: [CellMod e] -> CellScoreBuild e (Cell e) -> CellScoreBuild e (Cell e)
with' = with_ False

-- | Same as `with` but without any actions applied before running the score.
block :: CellScoreBuild e (Cell e) -> CellScoreBuild e (Cell e)
block = with []

-- | Same as `with'` but without any actions applied before running the score.
block' :: CellScoreBuild e (Cell e) -> CellScoreBuild e (Cell e)
block' = with' []

use_ :: Bool -> Bool -> Cell e -> [CellAct e] -> CellScoreBuild e (Cell e)
use_ keepNewTime keepState inputCell acts' = get >>= \(MkS3 stateCell _ _) -> let
  inputCell' = (if keepNewTime then inputCell { start = stateCell.start } else inputCell)
  in _gogo inputCell' inputCell' (if keepState then stateCell else inputCell') acts' _csbAddRetPut

-- | Add the input cell with some actions applied and set it as the new state. This will not
-- transfer the start time of the input cell; we use the state's current time instead.
use :: Cell e -> [CellAct e] -> CellScoreBuild e (Cell e)
use = use_ True False

-- | `use` that /does/ transfer the start time of the input cell.
use' :: Cell e -> [CellAct e] -> CellScoreBuild e (Cell e)
use' = use_ False False

-- | Add the input cell with some actions applied, but don't set it as the new state. Like
-- `use`, this will not transfer the start time of the input cell.
visit :: Cell e -> [CellAct e] -> CellScoreBuild e (Cell e)
visit = use_ True True

-- | `visit` that /does/ transfer the start time of the input cell.
visit' :: Cell e -> [CellAct e] -> CellScoreBuild e (Cell e)
visit' = use_ False True

-- | Repeat a score n times. The score is a function that takes the iteration as argument.
rep :: Int -> (Int -> CellScoreBuild e a) -> CellScoreBuild e ()
rep n f = mapM_ f [0..(n - 1)]

--------------------------------------------------------------------------------
-- ** Setter prefixes. A setter prefix takes a setter and an argument to form an action.

_act :: CellSetterActMode -> CellSetter e a -> a -> [CellAct e]
_act mode setter = map (MkS2 mode) . setter

-- | set a property in the added, returned, and new state cells.
go :: CellSetter e a -> a -> [CellAct e]
go = _act Go

-- | set a property only in the added, returned cells, but not the state cell.
at :: CellSetter e a -> a -> [CellAct e]
at = _act At

-- | set a property only in the added cell.
priv :: CellSetter e a -> a -> [CellAct e]
priv = _act Priv

-- | set a property only in the state cell.
andThen :: CellSetter e a -> a -> [CellAct e]
andThen = _act Then

ls :: a -> [a]
ls a = [a]

--------------------------------------------------------------------------------
-- ** Setters

-- | Replaces with a given cell except for the start time.
cell :: CellSetter e (Cell e)
cell x = ls $ \c -> x { start = c.start }

-- | Set frequency with an absolute hertz value.
freq :: CellSetter e Freq
freq x = ls $ \c -> case c of
  (MkC {}) -> c { freq = x }
  (MkAC {}) -> c { freqs = pure x }

-- | Set frequency with a ratio or scale factor.
itvl :: CellSetter e Freq
itvl x = ls $ \c -> case c of
  (MkC { freq = f }) -> c { freq = x * f }
  (MkAC { freqs = freqs }) -> c { freqs = x *| freqs }

-- | Set amplitude with an absolute amplitude value.
ampA :: CellSetter e Ampl
ampA x = ls $ \c -> case c of
  (MkC {}) -> c { ampl = x }
  (MkAC {}) -> c { ampls = pure x }

-- | Set amplitude with a ratio or scale factor.
ampX :: CellSetter e Ampl
ampX x = ls $ \c -> case c of
  (MkC { ampl = ampl }) -> c { ampl = x * ampl }
  (MkAC { ampls = ampls }) ->  c { ampls = x *| ampls }

-- | Set amplitude with a gain value (dB), which is translated into a scale factor.
gain :: CellSetter e Gain
gain x = ampX (gain2ampl x)

-- | Set start time.
jump :: CellSetter e TimeVal
jump x = ls $ \c -> c { start = x }

-- | Increment start time by adding the input multiplied by the state cell's duration.
step :: CellSetter e TimeVal
step x = ls $ \c -> c { start = c.start + x * c.dur }

-- | Set duration with an absolute time in seconds.
durA :: CellSetter e TimeVal
durA x = ls $ \c -> c { dur = x }

-- | Set duration with a scale factor.
durX :: CellSetter e TimeVal
durX x = ls $ \c -> c { dur = x * c.dur }

-- | Set pan with an absolute pan percentage (0 to 1, 0.5 is center)
panA :: CellSetter e Pan
panA x = ls $ \c -> case c of
  (MkC {}) -> c { pan = x }
  (MkAC {}) -> c { pans = pure x }

-- | Set pan with a scale factor.
panX :: CellSetter e Pan
panX x = ls $ \c -> case c of
  (MkC { pan = pan }) -> c { pan = x * pan }
  (MkAC { pans = pans }) -> c { pans = x *| pans }

-- | Set envelope to an `ADSRParams` value.
envl :: CellSetter e ADSRParams
envl x = ls $ \c -> case c of
  (MkC {}) -> c { adsr = x }
  (MkAC {}) -> c { env = adsr' x }

-- | Set envelope sustain value with a scale factor.
susX :: CellSetter e TimeVal
susX x = ls $ \c -> case c of
  (MkC { adsr = adsr } ) -> c { adsr = adsr { tS = x * adsr.tS } }
  (MkAC {}) -> error "cannot use susX with an articulated cell"

-- | \"Compiles\" a `CellScoreBuild` spec into song data playable with `retriggerWith` given
-- a starting cell state to interpret the score.
realizeCellScore  :: InstrumentSymbol -- ^ starting instrument name
                  -> Cell e  -- ^ starting cell state
                  -> [(InstrumentSymbol, Node e Now -> Node e (LR SynthVal))]  -- ^ map from instrument name to instrument
                  -> CellScoreBuild e a  -- ^ the cell score specification
                  -> [([Cell e], Node e Now -> Node e (LR SynthVal))]
realizeCellScore startInst startCell instrName2Node csb = let
  instrName2NodeMap = M.fromList instrName2Node
  s0 = MkS3 startCell startInst M.empty
  MkS3 _ _ score = execState csb s0
  in M.foldlWithKey' (\acc inst cellsThisInst -> case M.lookup inst instrName2NodeMap of
    Just nodeThisInst -> ((cellsThisInst, nodeThisInst) : acc)
    Nothing -> acc {-should probably report this with a message.. -}) [] score