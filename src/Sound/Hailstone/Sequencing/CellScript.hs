{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BangPatterns #-}

module Sound.Hailstone.Sequencing.CellScript
( CellScript
  -- ** Commands
, add, set, block, block', use, use', visit, visit', here, rep
  -- ** Instrument commands
, withInstr, setInstr
  -- ** Committers
  -- | A committer sets a field based on another cell.
, commitTime, commitDur, commitFreq, commitAmpl, commitPan, commitEnvl
  -- ** Setter prefixes
  -- | A setter prefix takes a setter and an argument to form an action.
, go, at, priv, andThen
  -- ** Setters
, freq, itvl, ampA, ampX, gain, jump, step, durA, durX, panA, panX, envl, susX, cell
  -- ** Compilation
, realizeCellScript
)
where

import qualified Data.Text as T
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import Sound.Hailstone.Sequencing.Cell
import Sound.Hailstone.Sequencing.Now (Now)
import Sound.Hailstone.Synth.Node (adsr', Node, LR)
import Sound.Hailstone.Synth.Effect (gain2ampl)
import Sound.Hailstone.Synth.SynthVal
import Sound.Hailstone.Synth.MiscTypes (ADSRParams(..), SPair(..), STriple(..))

type InstrumentSymbol = T.Text

type CellScore e = M.Map InstrumentSymbol [Cell e]

-- | An embedded DSL for writing a score (lists of `Cell`s and their instruments.)
type CellScript e a = State (STriple (Cell e) InstrumentSymbol (CellScore e)) a

_csPutAndMakeState :: Cell e -> Cell e -> InstrumentSymbol -> CellScore e -> STriple (Cell e) InstrumentSymbol (CellScore e)
_csPutAndMakeState addCell putCell inst score = MkS3 putCell inst $
  M.insert inst (addCell : (maybe [] id $ M.lookup inst score)) score

-- | add a cell (both add it to the score and return it) and put a cell in the state
_csAddRetPut :: Cell e -> Cell e -> Cell e -> CellScript e (Cell e)
_csAddRetPut addCell retCell putCell = state $ \(MkS3 _ inst score) -> (retCell, _csPutAndMakeState addCell putCell inst score)

-- | don't add anything to the score, just put a cell in the state and return it
_csRetPutThisOnly :: Cell e -> CellScript e (Cell e)
_csRetPutThisOnly putCell = state $ \(MkS3 _ inst score) -> (putCell, MkS3 putCell inst score)

data CellSetterActMode
  = At
    -- ^ apply the setter to the cell being added-and-returned only, not the state cell
  | Priv
    -- ^ apply the setter to the cell being added only and NOT the one being returned and of
    -- course also not the state cell
  | Go
    -- ^ apply the setter to the cell being added-&-returned AND the state cell
  | Then
    -- ^ don't apply the setter to the cell being added-and-returned; add it, then apply the
    -- setter to the current cell in the state

-- | a function that modifies a cell
type CellMod e = Cell e -> Cell e

-- | a setter takes an argument and returns a list of edits to be applied sequentially. This
-- list is usually just a singleton for most of the setter we provide; they will be composed
-- after they've been decorated with the `CellSetterActMode` in the form of `CellAct`s.
type CellSetter e a = a -> [CellMod e]

-- | a cell-modifying function paired with how to apply it on the state-put and added cells
type CellAct e = SPair CellSetterActMode (CellMod e)

--------------------------------------------------------------------------------
-- ** helper scalar-value (mnemonic: x) cell mods

-- | set can be written in terms of mod (const someval) but using fmap (const someval) on a node
-- is less efficient than `pure` which creates a constant emitting node, no fmapping needed
xsetFreq :: Freq -> CellMod e
xsetFreq x c = case c of
  MkC { } -> c { freq = x }
  MkAC { } -> c { freqs = pure x }

xmodFreq :: (Freq -> Freq) -> CellMod e
xmodFreq k c = case c of
  MkC { freq = x } -> c { freq = k x }
  MkAC { freqs = xs } -> c { freqs = fmap k xs }

xsetStart :: TimeVal -> CellMod e
xsetStart x c = c { start = x }

xsetDur :: TimeVal -> CellMod e
xsetDur x c = c { dur = x }

xmodDur :: (TimeVal -> TimeVal) -> CellMod e
xmodDur k c = c { dur = k c.dur }

xsetAmpl :: Ampl -> CellMod e
xsetAmpl x c = case c of
  MkC { } -> c { ampl = x }
  MkAC { } -> c { ampls = pure x }

xmodAmpl :: (Ampl -> Ampl) -> CellMod e
xmodAmpl k c = case c of
  MkC { ampl = x } -> c { ampl = k x }
  MkAC { ampls = xs } -> c { ampls = fmap k xs }

xsetPan :: Pan -> CellMod e
xsetPan x c = case c of
  MkC { } -> c { pan = x }
  MkAC { } -> c { pans = pure x }

xmodPan :: (Pan -> Pan) -> CellMod e
xmodPan k c = case c of
  MkC { pan = x } -> c { pan = k x }
  MkAC { pans = xs } -> c { pans = fmap k xs }

xsetADSR :: ADSRParams -> CellMod e
xsetADSR x c = case c of
  MkC { } -> c { adsr = x }
  MkAC { } -> c { env = adsr' x }

xmodADSR :: (ADSRParams -> ADSRParams) -> CellMod e
xmodADSR k c = case c of
  MkC { adsr = x } -> c { adsr = k x }
  MkAC { } -> error "cannot use an ADSR modifier function with an articulated cell"

--------------------------------------------------------------------------------
-- ** Committers (or copiers)
--
-- Sets a field based on another cell

type CellCopier e = Cell e -> Cell e -> Cell e

commitTime :: CellCopier e
commitTime cFrom cTo = xsetStart cFrom.start cTo

commitDur :: CellCopier e
commitDur cFrom cTo = xsetDur cFrom.dur cTo

commitFreq :: CellCopier e
commitFreq cFrom cTo = case cFrom of
  MkC { freq = x } -> xsetFreq x cTo
  MkAC { freqs = xs } -> case cTo of
    MkC { } -> lowerAC'toAC $ (promoteCtoAC' cTo) { freqs' = xs }
    MkAC { } -> cTo { freqs = xs }

commitAmpl :: CellCopier e
commitAmpl cFrom cTo = case cFrom of
  MkC { ampl = x } -> xsetAmpl x cTo
  MkAC { ampls = xs } -> case cTo of
    MkC { } -> lowerAC'toAC $ (promoteCtoAC' cTo) { ampls' = xs }
    MkAC { } -> cTo { ampls = xs }

commitPan :: CellCopier e
commitPan cFrom cTo = case cFrom of
  MkC { pan = x } -> xsetPan x cTo
  MkAC { pans = xs } -> case cTo of
    MkC { } -> lowerAC'toAC $ (promoteCtoAC' cTo) { pans' = xs }
    MkAC { } -> cTo { pans = xs }

commitEnvl :: CellCopier e
commitEnvl cFrom cTo = case cFrom of
  MkC { adsr = x } -> xsetADSR x cTo
  MkAC { env = xs } -> case cTo of
    MkC {  } -> lowerAC'toAC $ (promoteCtoAC' cTo) { env' = xs }
    MkAC { } -> cTo { env = xs }

--------------------------------------------------------------------------------
-- ** commands

-- | Run a cellscript with a given starting instrument; afterwards return to the instrument
-- in the state from before this command.
withInstr :: InstrumentSymbol -> CellScript e (Cell e) -> CellScript e (Cell e)
withInstr inst cs = state $ \(MkS3 stateCell origInst score) -> let
  (ret, (MkS3 stateCell' _ score')) = runState cs (MkS3 stateCell inst score)
  in (ret, MkS3 stateCell' origInst score')

-- | Switch to a new current instrument.
setInstr :: InstrumentSymbol -> CellScript e (Cell e)
setInstr inst = state $ \(MkS3 stateCell _ score) -> (stateCell, MkS3 stateCell inst score)

-- | processes lists of acts given the cell-to-add, cell-to-return, and cell-to-put-in-the-state
_gogo :: Cell e -> Cell e -> Cell e -> [CellAct e] -> (Cell e -> Cell e -> Cell e -> x) -> x
_gogo !addCell !retCell !putCell acts k = case acts of
  [] -> k addCell retCell putCell
  ((MkS2 mode f):as) -> case mode of
    Priv -> _gogo (f addCell) retCell     putCell     as k
    At ->   _gogo (f addCell) (f retCell) putCell     as k
    Go ->   _gogo (f addCell) (f retCell) (f putCell) as k
    Then -> _gogo addCell     retCell     (f putCell) as k

-- | Return the state cell at this point.
here :: CellScript e (Cell e)
here = (\(MkS3 stateCell _ _) -> stateCell) <$> get

-- | Add and return the state cell after doing some actions to it.
add :: [CellAct e] -> CellScript e (Cell e)
add acts' = get >>= \(MkS3 stateCell _ _) -> _gogo stateCell stateCell stateCell acts' _csAddRetPut

-- | Set properties of the state cell with some setters.
set :: [CellMod e] -> CellScript e (Cell e)
set fs = get >>= \(MkS3 stateCell _ _) -> _gogo stateCell stateCell stateCell
  (map (MkS2 Go) fs) (\_ _ -> _csRetPutThisOnly)

-- | Play a script, choosing which fields from the resulting state cell to \"commit\" to the
-- containing scope's state after the block plays.
block' :: [CellCopier e] -> CellScript e (Cell e) -> CellScript e (Cell e)
block' committers cs = state $ \s@(MkS3 stateCell _ _) -> let
  (ret, MkS3 stateCell' inst' score') = runState cs s
  stateCellWithCommits = foldr ($ stateCell') stateCell committers
  in (ret, MkS3 stateCellWithCommits inst' score')

-- | `block` that only commits the final start-time.
block :: CellScript e (Cell e) -> CellScript e (Cell e)
block = block' [commitTime]

use_ :: Bool -> Bool -> Cell e -> [CellAct e] -> CellScript e (Cell e)
use_ keepNewTime keepState inputCell acts' = get >>= \(MkS3 stateCell _ _) -> let
  inputCell' = (if keepNewTime then inputCell { start = stateCell.start } else inputCell)
  in _gogo inputCell' inputCell' (if keepState then stateCell else inputCell') acts' _csAddRetPut

-- | Add the input cell with some actions applied and set it as the new state. All fields of
-- the input cell are respected, except for the start time; we use the state's current time.
use :: Cell e -> [CellAct e] -> CellScript e (Cell e)
use = use_ True False

-- | `use` that /does/ make use of the start time of the input cell.
use' :: Cell e -> [CellAct e] -> CellScript e (Cell e)
use' = use_ False False

-- | Add the input cell with some actions applied, but don't set it as the new state. Like
-- `use`, this will not use the start time of the input cell, but state's current time.
visit :: Cell e -> [CellAct e] -> CellScript e (Cell e)
visit = use_ True True

-- | `visit` that /does/ make use of the start time of the input cell.
visit' :: Cell e -> [CellAct e] -> CellScript e (Cell e)
visit' = use_ False True

-- | Repeat a script n times. Takes a function that takes the iteration number as argument.
rep :: Int -> (Int -> CellScript e a) -> CellScript e ()
rep n f = mapM_ f [0..(n - 1)]

--------------------------------------------------------------------------------
-- ** Setter prefixes.
--
-- | A setter prefix takes a setter and an argument to form an action.

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
freq x = ls $ xsetFreq x

-- | Set frequency with a ratio or scale factor.
itvl :: CellSetter e Freq
itvl x = ls $ xmodFreq (x *)

-- | Set amplitude with an absolute amplitude value.
ampA :: CellSetter e Ampl
ampA x = ls $ xsetAmpl x

-- | Set amplitude with a ratio or scale factor.
ampX :: CellSetter e Ampl
ampX x = ls $ xmodAmpl (x *)

-- | Set amplitude with a gain value (dB), which is translated into a scale factor.
gain :: CellSetter e Gain
gain x = ampX (gain2ampl x)

-- | Set start time.
jump :: CellSetter e TimeVal
jump x = ls $ xsetStart x

-- | Increment start time by adding the input multiplied by the state cell's duration.
step :: CellSetter e TimeVal
step x = ls $ \c -> c { start = c.start + x * c.dur }

-- | Set duration with an absolute time in seconds.
durA :: CellSetter e TimeVal
durA x = ls $ xsetDur x

-- | Set duration with a scale factor.
durX :: CellSetter e TimeVal
durX x = ls $ xmodDur (x *)

-- | Set pan with an absolute pan percentage (0 to 1, 0.5 is center)
panA :: CellSetter e Pan
panA x = ls $ xsetPan x

-- | Set pan with a scale factor.
panX :: CellSetter e Pan
panX x = ls $ xmodPan (x *)

-- | Set envelope to an `ADSRParams` value.
envl :: CellSetter e ADSRParams
envl x = ls $ xsetADSR x

-- | Set envelope sustain value with a scale factor.
susX :: CellSetter e TimeVal
susX x = ls $ xmodADSR (\adsr -> adsr { tS = x * adsr.tS })
  -- ls $ \c -> case c of
  -- (MkC { adsr = adsr } ) -> c { adsr = adsr { tS = x * adsr.tS } }
  -- (MkAC {}) -> error "cannot use susX with an articulated cell"

-- | \"Compiles\" a `CellScript` spec into song data playable with `retriggerWith` given
-- a starting cell state to interpret the score.
realizeCellScript :: InstrumentSymbol -- ^ starting instrument name
                  -> Cell e  -- ^ starting cell state
                  -> [(InstrumentSymbol, Node e Now -> m (Node e (LR SynthVal)))]  -- ^ map from instrument name to instrument
                  -> CellScript e a  -- ^ the cell score specification
                  -> [([Cell e], Node e Now -> m (Node e (LR SynthVal)))]
realizeCellScript startInst startCell instrName2Node cs = let
  instrName2NodeMap = M.fromList instrName2Node
  s0 = MkS3 startCell startInst M.empty
  MkS3 _ _ score = execState cs s0
  in M.foldlWithKey' (\acc inst cellsThisInst -> case M.lookup inst instrName2NodeMap of
    Just nodeThisInst -> ((cellsThisInst, nodeThisInst) : acc)
    Nothing -> acc {-should probably report this with a message.. -}) [] score