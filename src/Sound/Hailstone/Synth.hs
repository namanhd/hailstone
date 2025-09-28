{-# LANGUAGE GADTs, BangPatterns, MultiWayIf, OverloadedRecordDot #-}

module Sound.Hailstone.Synth
( -- * Audio nodes
  --
  -- | An audio `Node` is a unit of computation with a state, and carries a signal function
  -- that reads and updates the state and emits a value. A `Node` can have child nodes whose
  -- emitted values are fed into the parent, forming a tree of nodes. Check the constructors
  -- for the minimal set of axiomatic nodes that cover our evaluation needs; check `mkNode1`
  -- `mkNode3` for some derived constructors with other arities.
  Node(..)
  -- *** Convenient derived constructors
, mkNode1, mkNode3
  -- ** Consuming nodes at the audio backend
, Sink(..), initSink, runNode, iterateNode
  -- ** Basic operators
, (|+|), (|*|), (+|), (*|), accum, tick
  -- ** Numeric and stereo conversions
, roundClip, asPCM, stereoize, stereoizeN, m2s
  -- ** Sequencing
, startAt, piecewise, cascade
  -- *** Retriggering with `Cell`s
, EnvelopeCellDurationMode(..), RetriggerMode(..)
, retriggerWith
  -- ** Envelopes
, ADSRParams(..)
, mkADSRez
, adsrEnvelope, adsrEnvelopeN
, funcRamp, linearRamp
  -- ** Generators
, sinOsc, sinOscP
  -- * Re-exports
, SynthVal, Freq, Gain, Pan, SampleRate, TimeVal, SampleVal
, ChanMode(..)
, Cell(..)
, LR(..)
)
where

import Data.List (sortOn)
import Data.Functor ((<&>))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Sound.Hailstone.Types

--------------------------------------------------------------------------------
-- * Signal functions

-- | A signal function will need access to the current time, as well as the time
-- delta between samplings (i.e. 1 / (samplerate))
type SigEnv = (TimeVal, TimeVal)

-- | Signal function: a signal function reads the signal environment @r@ (e.g., to get time,
-- delta time) in addition to an argument @a@, emits a value @x@, and updates the state @s@.
type SFr r a s x = ReaderT (r, a) (State s) x

-- | Signal function with @r@ specialized to be `SigEnv` since that's what we always use.
type SF a s x = SFr SigEnv a s x

-- | Ask for just the arguments from the reader context.
sfArgs :: SF a s a
sfArgs = reader snd
{-# INLINABLE sfArgs #-}

-- | Ask for just the signal environment from the reader context.
sfEnv :: SF a s SigEnv
sfEnv = reader fst
{-# INLINABLE sfEnv #-}

-- | Ask for just the time from the reader context.
sfTime :: SF a s TimeVal
sfTime = reader $ fst . fst
{-# INLINABLE sfTime #-}

-- | Run a signal function given an initial state, signal environment, and argument.
runSF :: SF a s x -> SigEnv -> a -> s -> (x, s)
runSF !sig !r !a = runState $ runReaderT sig (r, a)
{-# INLINABLE runSF #-}

-- | Embed a pure signal function as an `SFr`.
sfr :: (r -> a -> s -> (x, s)) -> SFr r a s x
sfr !f = ask >>= \(!r, !a) -> state $ f r a
{-# INLINABLE sfr #-}

--------------------------------------------------------------------------------
-- * Audio nodes

{-|
Audio Node tree; each node packs a state (of whatever state type @s@) together with a signal
function' that can operate on that state to emit a value and a new state.  (The constructors
are for different arity cases. We only need up to 2, then arities higher than two are easily
decomposed into binary functions). As a result, this is a mostly applicative interface, with
a natural applicative instance. (Show s currently used for debugging; can remove this later)

There are a number of design decisions here...
- Q: Why can't we get away with just a `MkNode0` that can do everything?
    - A: While it's true that you can write an Applicative instance such that any n-ary func
      can just be represented as a MkNode0 that emits that function applied to other MkNode0
      that emit arguments, such a function will __not__ be able to modify the state based on
      these arguments. (i.e. the function is confined to the "pure slot" of the applicative,
      and cannot "reach outwards" and affect the node state, a necessity for modeling e.g. a
      sine oscillator carrying an angle state (which must update node state based on emitted
      frequency values from an argument node.)
- Q: Why do we need `MkNode2`? Why not @MkNode1@, since the @a@ in @`SF` a s x@ could be any
  compound type, including a tuple such as (f, a) like what's seen with `MkNode2`?
    - A: It turns out that you can model a great many things with a two-argument-taking node
      especially function application, allowing an almost trivial applicative instance, even
      more efficient than certain <*> special cases. Attempting to derive mkNode2 & the appl
      instance from only MkNode1 would involve not only spawning more nodes that do plumbing
      but it would necessarily involve something along the lines of doing @(,) <$> aNode <*>
      bNode@, (i.e. wrapping up two nodes into a single tuple-emitting node), but that turns
      out to need the (<*>) we were trying to write in the first place, or needs to be hard-
      coded, which does not feel quite right. (Maybe there is a good way to use just MkNode1
      though, I haven't thought everything through.)
-}
data Node x where
  MkNode0 :: (Show s) => !s -> !(SF () s x) -> Node x
  -- ^nullary node, representing a source of values with no arguments needed.
  MkNode2 :: (Show s) => !s -> !(SF (f, a) s x) -> !(Node f) -> !(Node a) -> Node x
  -- ^binary node. can represent any time-varying computation taking two argument nodes, but
  -- can also be used to lift, into a node, via the applicative instance, the application of
  -- a pure function @f@ to a pure argument @a@.

  -- TODO: implement Let and Var nodes for reusing computed samples (this is on the way to
  -- 'observable sharing' for DSLs to hijack the host language's let mechanisms to get
  -- sharing for itself, but it's black magic, not sure if I want to do that, esp. since
  -- i'll be writing the user-facing language separately too, which will need Let and Var in any case)
  -- https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/doc/design/design.md


instance Functor Node where
  fmap f (MkNode0 !s !sig) = MkNode0 s (fmap f sig)
  fmap f (MkNode2 !s !sig !aNode !bNode) = MkNode2 s (fmap f sig) aNode bNode

instance Applicative Node where
  pure = MkNode0 () . pure
  {-# INLINABLE pure #-}
  liftA2 !f !aNode !bNode = MkNode2 () (sfArgs >>= \(!a, !b) -> pure (f a b)) aNode bNode
  {-# INLINABLE liftA2 #-}
  !fNode <*> !xNode = MkNode2 () (sfArgs >>= \(!f, !x) -> pure (f x)) fNode xNode
  {-# INLINABLE (<*>) #-}

showState :: (Show s) => s -> String
showState s = "{" <> editShowState (show s) <> "}"
  where editShowState str = if str == "()" then "" else str

instance Show (Node x) where
  show node = let
    editShowNode str = if str == "()" then "_" else "(" <> str <> ")"
    showBinNode aNode bNode = editShowNode (show aNode) <> " " <> editShowNode (show bNode)
    in case node of
      (MkNode0 s _) -> let str = "N0 " <> showState s in if str == "N0 {}" then "" else str
      (MkNode2 s _ aNode bNode) -> "N2 " <> showState s <> " " <> showBinNode aNode bNode

instance (Num a) => Num (Node a) where
  -- these two we use our own functions with inline annotations
  (+) = (|+|)
  {-# INLINABLE (+) #-}
  (*) = (|*|)
  {-# INLINABLE (*) #-}
  (-) = liftA2 (-)
  {-# INLINABLE (-) #-}
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Node a) where
  (/) = liftA2 (/)
  {-# INLINABLE (/) #-}
  recip = fmap recip
  fromRational = pure . fromRational

instance (Floating a) => Floating (Node a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

--------------------------------------------------------------------------------
-- *** Convenient derived constructors

-- | Constructor for a unary node, derived from `MkNode2`.
mkNode1 :: Show s => s -> SF a s x -> Node a -> Node x
mkNode1 s sig node = MkNode2 s (sfr $ \r (a, _) -> runSF sig r a) node (pure ())
{-# INLINABLE mkNode1 #-}

-- | Constructor for a ternary node, derived from `MkNode2`. (This pattern of tupling up arg
-- nodes can be extended indefinitely to higher arities if we ever need them.)
mkNode3 :: Show s => s -> SF (a, b, c) s x -> Node a -> Node b -> Node c -> Node x
mkNode3 s sig aNode bNode = MkNode2 s (sfr $ \r ((a, b), c) -> runSF sig r (a, b, c))
  $ liftA2 (,) aNode bNode

--------------------------------------------------------------------------------
-- ** Consuming nodes at the audio backend

-- | Run the audio node tree one step, given a signal environment. Returns the emitted value
-- and an updated tree (with the new node states as returned by the signal functions). (Note
-- that profiling indicates that we are truly licensed to have a ton of seqs & bang patterns
-- which provide a great and measurable benefit in reducing thunks, allocations & GC stops.)
runNode :: SigEnv -> Node x -> (x, Node x)
runNode !r = go
  where
    go :: Node x -> (x, Node x)
    go (MkNode0 !s !sig) = let
      (!x, !new_s) = runSF sig r () s in x `seq` new_s `seq` (x, MkNode0 new_s sig)
    go (MkNode2 !s !sig !aNode !bNode) = let
      (!a, !new_aNode) = aNode `seq` go aNode
      (!b, !new_bNode) = bNode `seq` go bNode
      (!x, !new_s) = a `seq` b `seq` runSF sig r (a, b) s
      in x `seq` new_aNode `seq` new_bNode `seq`
        (x, MkNode2 new_s sig new_aNode new_bNode)

-- | Queries a node @n@ times given the signal environment and a `SigEnv`-stepping function.
-- This gives a very simple way to sample the node tree and produce an array of samples, but
-- a list probably won't be very fast for downstream uses.
iterateNode :: SigEnv -> (SigEnv -> SigEnv) -> Int -> Node x -> ([x], Node x)
iterateNode r stepper n node = if n <= 0 then ([], node) else let
  (x, nodeNext) = runNode r node
  (xs, nodeFinal) = iterateNode (stepper r) stepper (pred n) nodeNext
  in (x : xs, nodeFinal)

-- | A convenience state type for an audio backend to save. getDestNode, getCurrTime usually
-- are what gets consumed and updated every time a sample is queried from the node tree, tho
-- the specifics of how this will be consumed and samples written will be up to the backend.
data Sink =
  MkSink  { getDestNode :: !(Node (LR SampleVal))
          , getCurrTime :: !TimeVal
          , getDeltaTime :: !TimeVal
          , getChanMode :: !ChanMode
          }

-- | Initializing a `Sink` with a pure zero `Node`.
initSink :: SampleRate -> ChanMode -> Sink
initSink sampleRate chanMode =
  MkSink  { getDestNode = pure 0
          , getCurrTime = 0.0
          , getDeltaTime = recip $ fromIntegral sampleRate
          , getChanMode = chanMode
          }

--------------------------------------------------------------------------------
-- ** Basic operators

-- | Generic accumulator node, with a `start` state plus an accumulator `accumFn`. The state
-- updates every time a value is emitted; the emitted value is that state.
accum :: Show s => s -> (i -> s -> s) -> Node i -> Node s
accum start accumFn = mkNode1 start $ sfArgs >>= \i -> modify (accumFn i) *> get

-- | Simple counter/ticker, from a `start` value and an `increment` from a node.  That state
-- is then emitted.
tick :: (Num a, Show a) => a -> Node a -> Node a
tick start = mkNode1 start $ sfArgs >>= \i -> state $ \s -> (s, s + i)

-- test stuff
-- __testenv = (0.0 :: TimeVal, 1.0 :: TimeVal)

-- __stepper :: SigEnv -> SigEnv
-- __stepper = (\(t, d) -> (t + d, d))

-- __testApplic = iterateNode __testenv id 3 $ (\a b c -> a + b + c) <$> (tick 1 1) <*> (tick 2 1) <*> (tick 3 1)
-- __testPiecewise = iterateNode __testenv __stepper 14 $
--   startAt (-1) 2.0 (Just 5) $ piecewise 1.0 0 [(tick 10 1, 3), (tick 20 1, 2), (tick 30 1, 4)]

-- | Retrieve the time tick context from the signal environment, as a node.
nTime :: Node TimeVal
nTime = MkNode0 () sfTime
{-# INLINABLE nTime #-}

-- | Mix two nodes with addition.
(|+|) :: (Num a) => Node a -> Node a -> Node a
{-# SPECIALIZE (|+|) :: Node SynthVal -> Node SynthVal -> Node SynthVal #-}
(|+|) = liftA2 (+)
{-# INLINABLE (|+|) #-}
infixl 3 |+|

-- | Since we have `|+|`, we might as well have `|*|` which multiplies two nodes pointwise.
(|*|) :: (Num a) => Node a -> Node a -> Node a
{-# SPECIALIZE (|*|) :: Node SynthVal -> Node SynthVal -> Node SynthVal #-}
(|*|) = liftA2 (*)
{-# INLINABLE (|*|) #-}
infixl 4 |*|

-- | Add to a node a scalar addend.
(+|) :: (Num a) => a -> Node a -> Node a
{-# SPECIALIZE[0] (+|) :: SynthVal -> Node SynthVal -> Node SynthVal #-}
(+|) addend = fmap (addend +)
infixr 6 +|
{-# INLINABLE (+|) #-}

-- | Scale a node by a scalar multiplier.
(*|) :: (Num a) => a -> Node a -> Node a
{-# SPECIALIZE (*|) :: SynthVal -> Node SynthVal -> Node SynthVal #-}
(*|) multiplier = fmap (multiplier *)
infixr 7 *|
{-# INLINABLE (*|) #-}

-- | Warp the time axis for a node using a function that modifies time AND its derivative to
-- modify deltaTime. (If only we could find the derivative automatically?)
modifyTime :: (TimeVal -> TimeVal) -> (TimeVal -> TimeVal) -> Node a -> Node a
modifyTime f df node = case node of
  (MkNode0 s sig) -> MkNode0 s (sfModifyTime f df sig)
  -- must recursively modify time for the child nodes too since our signal function concerns
  -- only ourselves; the child nodes are evaluated before we are.
  (MkNode2 s sig aNode bNode) -> MkNode2 s (sfModifyTime f df sig) (modifyTime f df aNode) (modifyTime f df bNode)
  where
    sfModifyTime :: (TimeVal -> TimeVal) -> (TimeVal -> TimeVal) -> SF a s x -> SF a s x
    sfModifyTime g dg = local (\((t, d), ar) -> ((g t, dg d), ar))

-- | Have a node start emitting at a start time (and possibly end after some duration), else
-- emitting a default empty value outside the allowed timespan.
startAt :: a -> TimeVal -> Maybe TimeVal -> Node a -> Node a
startAt empt start mdur node' = MkNode0 (modifyTime (subtract start) id node')
  (sfr $ \r@(t, _) _ node -> let nop = (empt, node) in if t < start then nop else case mdur of
    Nothing -> runNode r node; Just dur -> if t < (start + dur) then runNode r node else nop)

--------------------------------------------------------------------------------
-- ** Numeric and stereo conversions

-- | Round and clip a node's output to the bounds allowed by the sample format.
roundClip :: (RealFrac v, Bounded samp, Ord samp, Integral samp) => Node (LR v) -> Node (LR samp)
{-# SPECIALIZE roundClip :: Node (LR SynthVal) -> Node (LR SampleVal) #-}
roundClip = fmap (fmap (min maxBound . max minBound . round))

-- | Convert a double-generating signal into a signal of sample values expected by our audio
-- setup (here, @Int16@), with scaling up to sample values followed by rounding and clipping
asPCM :: RealFrac v => Node (LR v) -> Node (LR SampleVal)
{-# SPECIALIZE asPCM :: Node (LR SynthVal) -> Node (LR SampleVal) #-}
asPCM = roundClip . (*|) (fromIntegral (maxBound :: SampleVal))

-- | Apply some panning to turn a mono signal into a stereo signal, or modify the panning of
-- an existing stereo signal. Note that pan values are from 0 to 1 where 0 is hard left, 0.5
-- is centered, and 1.0 is hard right.  (Though, no bounds checking is done to ensure this.)
-- See `stereoize` for a version where the pan value can be emitted from an argument `Node`.
stereoize :: (Num a) => a -> Either (Node a) (Node (LR a)) -> Node (LR a)
stereoize pan (Left monoNode) = fmap (\a -> MkLR (((1 - pan) * a), (pan * a))) monoNode
stereoize pan (Right stereoNode) = fmap (\(MkLR (al, ar)) -> let a = al + ar in MkLR (((1 - pan) * a), (pan * a))) stereoNode

-- | The node-parameterized version of `stereoize`, where pan values may be from a node and
-- not constant.
stereoizeN :: (Num a) => Node a -> Either (Node a) (Node (LR a)) -> Node (LR a)
stereoizeN panNode (Left monoNode) = liftA2 (\pan a -> MkLR (((1 - pan) * a), (pan * a))) panNode monoNode
stereoizeN panNode (Right stereoNode) = liftA2 (\pan (MkLR (al, ar)) -> let a = al + ar in MkLR (((1 - pan) * a), (pan * a))) panNode stereoNode

-- | mono2stereo. Convenience to turn a mono real-valued signal into a stereo signal.
m2s :: (Fractional a) => Node a -> Node (LR a)
m2s = stereoize 0.5 . Left

--------------------------------------------------------------------------------
-- ** Sequencing

-- | Sequence nodes together monophonically, given their start times.
piecewise :: TimeVal
          -- ^the start time for this sequence of nodes
          -> a -- ^empty value to emit when no nodes are scheduled to be playing
          -> [(Node a, TimeVal, Maybe TimeVal)]
          -- ^a list of @[(node, start, maybeDuration)]@; @node@ starts at @start@ and plays
          -- until the next node with the next start time. @maybeDuration@ is ignored except
          -- for the last node in start order!!
          -> Node a
piecewise t0 empt nodesDurs = out
  where
  -- sort by start time, apply (t0+start) offsets to everyone
  nodesDurs' =
    fmap (\(node, sta, mdur) -> let sta' = sta + t0
      in (startAt empt sta' mdur node, sta', mdur)) $ sortOn (\(_, sta, _) -> sta) nodesDurs
  -- get initial state and form the output node
  out = case nodesDurs' of
    ((firstNode,firstStartTime,firstMDur):restNodes) -> MkNode0
      (firstNode, firstStartTime, firstMDur, restNodes)
      (sfEnv >>= \r@(t, _) -> state $ customMap t r)
    [] -> pure empt
  -- the state function for the node. check the start time of the next node and switch to it
  -- and consume one node from the list if its time is here, otherwise just keep playing the
  -- current node
  customMap t r (!currNode, !currStartTime, !currMDur, nil@[]) = let
    (x, new_currNode) = case currMDur of
      Nothing -> runNode r currNode
      Just dur -> if t < currStartTime + dur then runNode r currNode else (empt, currNode)
    in (x, (new_currNode, currStartTime, currMDur, nil))
  customMap t r s@(currNode, !currStartTime, currMDur, ls@((nextNode, !nextStartTime, nextMDur):rest))
    | t < currStartTime = (empt, s)
    | t < nextStartTime = let (x, new_currNode) = runNode r currNode in (x, (new_currNode, currStartTime, currMDur, ls))
    | otherwise = nextNode `seq` nextStartTime `seq` nextMDur `seq` rest `seq`
      customMap t r (nextNode, nextStartTime, nextMDur, rest)

-- | Given a list @[(node, start, dur)]@, sequence them and let them to play over each other
-- i.e. if the list says @node0@ starts at t=0s and plays for 5s, and @node1@ starts at t=2s
-- and plays for 6s, then both should play together in the interval 2 <= t < 5. Makes use of
-- a function to merge values from simultaneously playing nodes, and an empty value, (so @a@
-- actually just needs to be a @Monoid@ rather than a full @Num@, but it is real annoying to
-- wrap and unwrap the numeric @Sum@ monoid newtype so we'll just expect @Num@, won't hurt.)
cascade :: (Foldable f, Functor f, Num a, Show (f (Node a)))
        => TimeVal -- ^the start time for this sequence of nodes
        -> a -- ^empty value to emit when no nodes are scheduled to be playing
        -> f (Node a, TimeVal, Maybe TimeVal)
          -- ^a list of @[(node, start, maybe duration)]@; @node@ starts at @start@ and
          -- plays for @duration@ if specified, sequenced
        -> Node a
cascade t0 empt = sum . fmap (\(node, start, mdur) -> startAt empt (t0 + start) mdur node)

-- | A setting for `retriggerWith` determining whether to cut the cell's envelope when we're
-- at its duration (which is `EnvelopeCutsAtCellDuration`) or let the envelope finish.
data EnvelopeCellDurationMode = EnvelopeIgnoresCellDuration | EnvelopeCutsAtCellDuration
  deriving (Eq, Show, Ord)

-- | A setting for `retriggerWith` determining if the cells will be monophonic or polyphonic
-- (i.e. cells can play over each other) as cells do specify their start time & can overlap.
-- If `RetrigMonophonic`, we'll sort the cell list by start time and queue them back to back
-- ignoring all cell durations except for the last one.  If `RetrigPolyphonic`, we'll simply
-- schedule all cells to start at their specified start (and possibly end) time, and summing
-- up the resulting nodes (letting them play over each other freely.)
data RetriggerMode = RetrigMonophonic | RetrigPolyphonic
  deriving (Eq, Show, Ord)

-- | Play a melody (a list of `Cell`) with a synth (a node parameterized by @`Node` `Freq`@,
-- @`Node` `Gain`@). Effectively "resets and retriggers" a node in sync with note data. Also
-- panning will be applied, since a note `Cell`s may specify pan.
retriggerWith :: EnvelopeCellDurationMode
                -- ^how to treat envelope vs. cell duration, see docs for this type
              -> RetriggerMode -- ^monophonic or polyphonic triggering
              -> TimeVal -- ^a start time for this retriggering sequence
              -> LR SynthVal -- ^empty value for when notes have concluded
              -> (Node Freq -> Node Gain -> Node (LR SynthVal))
                -- ^instrument to retrigger parameterized by frequency & gain
              -> [Cell] -- ^note data cells making up the melody
              -> Node (LR SynthVal)
retriggerWith envCellDurMode retrigMode t0 empt instrument cells = ff t0 empt nodesDurs
  where
    ff = case retrigMode of
      RetrigMonophonic -> piecewise
      RetrigPolyphonic -> cascade
    doPanning maybePan = stereoize (unmaybePan maybePan) . Right
    nodesDurs = cells <&> (\cell -> let
      adsrVals = cell._adsr
      dur = case envCellDurMode of
        EnvelopeIgnoresCellDuration -> max (adsrTotalTime adsrVals) cell._dur
        EnvelopeCutsAtCellDuration -> cell._dur
      node = (fmap dupLR (adsrEnvelope adsrVals) |*|)
        $ doPanning cell._pan
        $ instrument (pure cell._freq) (pure cell._gain)
      in (node, cell._start, Just dur))

--------------------------------------------------------------------------------
-- ** Envelopes

-- | Pure function for `funcRamp`
_funcRamp_fn :: (Num v, Ord v) => (TimeVal -> v) -> TimeVal -> v -> v -> TimeVal -> v
_funcRamp_fn f dur start end t = let
  downramp = start > end
  smaller = min start end
  bigger = max start end
  up = max smaller . min bigger $ smaller + (bigger - smaller) * (f $ t / dur)
  in if downramp then smaller + bigger - up else up

-- | A ramp signal that increases from start to end (or possibly decreases) in a given range
-- of time based on some given function, then holds. For linear ramping, the function should
-- be \t -> t; For a quadratic ramp, the function is \t -> t*t etc.
funcRamp :: (Num v, Ord v) => (TimeVal -> v) -> TimeVal -> v -> v -> Node v
funcRamp f dur start end = if dur == 0 then pure end else nTime <&> _funcRamp_fn f dur start end

-- | Specialization of funcRamp to a linear function
linearRamp :: TimeVal -> SynthVal -> SynthVal -> Node SynthVal
linearRamp = funcRamp id

-- | Create a signal traversing an ADSR envelope according to some fixed envelope params.
adsrEnvelope :: ADSRParams -> Node SynthVal
adsrEnvelope (ADSR v0 tA tD v1 tS tR v2) = let
  !dA = (1.0 - v0) / tA
  !dD = (v1 - 1.0) / tD
  !t0S = tA + tD
  !t0R = t0S + tS
  !dR = (v2 - v1) / tR
  !tEnd = t0R + tR
  in MkNode0 () . (sfTime <&>) $ \t -> if
    | t < tA -> v0 + t * dA
    | t < t0S -> 1.0 + (t - tA) * dD
    | t < t0R -> v1
    | t < tEnd -> v1 + (t - t0R) * dR
    | otherwise -> v2

-- | A node-parameterized version of `adsrEnvelope` which can take `ADSRParams` emitted from
-- a node. Note that this works fairly differently from `adsrEnvelope`: ADSR parameters here
-- are variable so we have to keep the envelope state, incrementing it with a slope computed
-- using the latest received ADSR parameters.
adsrEnvelopeN :: Node ADSRParams -> Node SynthVal
adsrEnvelopeN = mkNode1 (read "NaN" :: SynthVal, Nothing) . sfr $
  \(t, d) (ADSR v0 tA tD v1 tS tR v2) (!x', !precalcs') -> let
    -- using NaN as a (Nothing :: Maybe Double) to not have to waste resources on Just unbox
    -- but this won't generalize to all choices of SynthVal...  but realistically it's fine?
    x = if isNaN x' then v0 else x'
    precalcs = maybe
      ( d * (1.0 - v0) / tA
      , d * (v1 - 1.0) / tD
      , tA + tD
      , tA + tD + tS
      , d * (v2 - v1) / tR
      , tA + tD + tS + tR
      ) id precalcs'
    (mA, mD, t0S, t0R, mR, tEnd) = precalcs
    -- (deltaTime * attackSlope, deltaTime * decaySlope, sustainStartT, releaseStartT, deltaTime * releaseSlope)
    newX = if | t < tA -> x + mA
              | t < t0S -> x + mD
              | t < t0R -> x
              | t < tEnd -> x + mR
              | otherwise -> v2
    newS = (newX, if precalcs' == Nothing then Just precalcs else precalcs')
    -- clamp output to be between 0 and 1
    in (newX, newS)

--------------------------------------------------------------------------------
-- ** Generators

twopi :: SynthVal
twopi = 2 * pi

-- | Shared signal function for sin oscillators.
-- Based on https://juce.com/tutorials/tutorial_sine_synth/
_sinOsc_fn :: TimeVal -> Freq -> Gain -> SynthVal -> SynthVal -> (SynthVal, SynthVal)
_sinOsc_fn d f gain phase myAngle = let
  angleDelta = f * (d * twopi)
  newAngle = myAngle + angleDelta
  newAngleNormed = if newAngle > twopi then newAngle - twopi else newAngle
  in (gain * sin (myAngle + phase), newAngleNormed)
{-# INLINE _sinOsc_fn #-}

_sinOsc_s0 :: SynthVal
_sinOsc_s0 = 0.0
{-# INLINE _sinOsc_s0 #-}

_sinOsc_SF :: SF (Freq, Gain) SynthVal SynthVal
_sinOsc_SF = sfr $ \(_, d) (f, gain) myAngle -> _sinOsc_fn d f gain 0 myAngle
{-# INLINE _sinOsc_SF #-}

_sinOscP_SF :: SF (SynthVal, Freq, Gain) SynthVal SynthVal
_sinOscP_SF = sfr $ \(_, d) (phase, f, gain) myAngle -> _sinOsc_fn d f gain phase myAngle
{-# INLINE _sinOscP_SF #-}

-- | Sine oscillator, holding state for the current angle.
sinOsc :: Node Freq -> Node Gain -> Node SynthVal
sinOsc = MkNode2 _sinOsc_s0 _sinOsc_SF

-- | `sinOsc` but accepts phase as an input node (added to the angle of each sin compute).
sinOscP :: Node SynthVal -> Node Freq -> Node Gain  -> Node SynthVal
sinOscP = mkNode3 _sinOsc_s0 _sinOscP_SF
