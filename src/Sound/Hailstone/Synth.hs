{-# LANGUAGE Strict #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- for HasField passthrough
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-} -- needed for the HasField instance to be usable, apparently
{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Hailstone.Synth
( -- * Audio nodes
  --
  -- | An audio `Node` is a unit of computation with a state, and carries a signal function
  -- that reads and updates the state and emits a value. A `Node` can have child nodes whose
  -- emitted values are fed into the parent, forming a tree of nodes.
  --
  -- Check the constructors for the minimal set of axiomatic nodes that cover our evaluation
  -- needs; check `mkNode1` `mkNode3` for some derived constructors with other arities.
  --
  -- The audio node /tree/ can become an audio node /graph/ (a DAG, to be specific) with the
  -- use of `share`, which uses sorcery to enable /observable sharing/, letting the embedded
  -- DSL see the sharing semantics, the let-bindings & usage of names, of the host language.
  Node(..)
  -- *** Convenient derived constructors
, mkNode1, mkNode1IO, mkNode3, mkNode4_
  -- ** Observable sharing
, share
  -- ** Consuming nodes at the audio backend
, Sink(..), initSink, runNode, iterateNode
  -- ** Basic operators
, (|+|), (|*|), (+|), (+||), (*|), (*||), accum, tick
  -- ** Numeric and stereo conversions
, asPCM, stereodup, monosum, mono2stereo, stereo2mono, mapLRNode, balance
  -- ** Sequencing
, startAt, piecewiseMono, piecewisePoly, cascade
  -- ** Envelopes
, adsr', adsr'N, adsr
, funcRamp, linearRamp
  -- ** Generators
, sinOsc, sinOscPM, sqrOsc, sqrOscDM, sawOsc, triOsc, triOscPM, sawOscSM
  -- ** Effects
, gain, gainN
, echoRaw, echo, echo'
  -- *** Filters à la RBJ's audio EQ cookbook
, lpf, lpfN
  -- * Re-exports
, module Sound.Hailstone.Types
)
where

import Prelude hiding (isNaN)
import Data.Ord (clamp)
import Data.Functor ((<&>))
import Sound.Hailstone.Types
import qualified Sound.Hailstone.VoiceAlloc as VoiceAlloc
import qualified Sound.Hailstone.DelayQueue as DelayQueue

-- dark magic for observable sharing
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef)

-- for HasField passthrough
import GHC.Records

-- | A signal function needs access to the current time and the time delta between samplings
-- (i.e. 1 / (samplerate)). We also take an external environment type @e@ along for the ride
-- for other outside-world information (loaded waveform/sample buffers, MIDI messages, etc.)
type SigEnv e = (TimeVal, TimeVal, e)

--------------------------------------------------------------------------------
-- * Audio nodes

{-|
Audio Node tree; each node packs a state (of whatever state type @s@) together with a signal
function that can operate on that state to emit a value and a new state. The type @e@ is for
any outside-world information that nodes need access to.

(These constructors are for different arity cases. We only need up to 2, then arities higher
than two are easily decomposed into binary functions).

There are a number of design decisions here...

- Q: Why can't we get away with just a `MkNode0` that can do everything?

    - A: While it's true that you can write an Applicative instance such that any n-ary func
      can just be represented as a MkNode0 that emits that function applied to other MkNode0
      that emit arguments, such a function will __not__ be able to modify the state based on
      these arguments. (i.e. the function is confined to the applicative's \"pure slot\" and
      cannot \"reach outwards\" and affect the node state, a necessity for modeling e.g. the
      sine oscillator carrying an angle state (which must update node state based on emitted
      frequency values from an argument node.)

- Q: Why do we need `MkNode2`? Why not @MkNode1@, since the @a@ in @`SF` a s x@ could be any
  compound type, including a tuple such as @(f, a)@ like what's seen with `MkNode2`?

    - A: It turns out you can model a great many things with a two-argument node, especially
      function application, plus allowing an almost trivial applicative instance w/ @liftA2@
      and @\<*\>@ both admitting efficient definitions. To write @mkNode2@ & the applicative
      from only @MkNode1@ would involve spawning new nodes that do plumbing plus hard-coding
      @(,) \<$\> aNode \<*\> bNode@ (i.e. wrapping up two nodes into one tuple-emitting node
      in order to get tree-like branching via tuple types.)

- Q: This is a binary tree... One usually hears of audio graphs, not audio node trees. Is it
  possible to represent audio node graphs i.e. forking a node output to feed multiple nodes?

    - A: With some cursed magic to implement __observable sharing__ in `share`, yes. That is
      the general way to hijack the host language's sharing and make it manifest in the EDSL
      embedded within it, without having to make the DSL type a monad (which we don't have).

- Q: Why are there separate stateless variants of `MkNode0` and `MkNode2`?

    - A: We can model `MkNode0_` and `MkNode2_` with the stateful counterparts, but this has
      some cost (the dummy state @()@ being created or passed around for function calls). As
      we're trying to be as efficient as possible, we specialize as much as possible.
-}
data Node e x where
  MkNodeConst :: !x -> Node e x
  -- ^constant node of a pure value. can be done with just `MkNode0`, but having a dedicated
  -- constructor for this common case should be more efficient.
  MkNode0 :: !s -> !(SigEnv e -> s -> SPair x s) -> Node e x
  -- ^stateful nullary node, a source of values with no arguments needed.
  MkNode0_ :: !(SigEnv e -> x) -> Node e x
  -- ^stateless nullary node.
  MkNode2 :: !s -> !(SigEnv e -> a -> b -> s -> SPair x s) -> ~(Node e a) -> ~(Node e b) -> Node e x
  -- ^stateful binary node, taking two argument nodes.
  MkNode2_ :: !(SigEnv e -> a -> b -> x) -> ~(Node e a) -> ~(Node e b) -> Node e x
  -- ^stateless binary node.
  MkNode0IO :: !s -> !(SigEnv e -> s -> IO (SPair x s)) -> Node e x
  -- ^IO-effectful stateful nullary node. Use sparingly!
  MkNode2IO :: !s -> !(SigEnv e -> a -> b -> s -> IO (SPair x s)) -> ~(Node e a) -> ~(Node e b) -> Node e x
  -- ^IO-effectful stateful binary node. Use sparingly!

instance Functor (Node e) where
  fmap f (MkNodeConst x) = MkNodeConst (f x)
  fmap f (MkNode0 s sig) = MkNode0 s
    (\r s' -> let MkS2 x s'' = sig r s' in MkS2 (f x) s'')
  fmap f (MkNode0_ sig) = MkNode0_
    (\r -> f (sig r))
  fmap f (MkNode2 s sig aNode bNode) = MkNode2 s
    (\r a b s' -> let MkS2 x s'' = sig r a b s' in MkS2 (f x) s'') aNode bNode
  fmap f (MkNode2_ sig aNode bNode) = MkNode2_
    (\r a b -> f (sig r a b)) aNode bNode
  fmap f (MkNode0IO s sig) = MkNode0IO s
    (\r s' -> sig r s' <&> \(MkS2 x s'') -> MkS2 (f x) s'')
  fmap f (MkNode2IO s sig aNode bNode) = MkNode2IO s
    (\r a b s' -> sig r a b s' <&> \(MkS2 x s'') -> MkS2 (f x) s'') aNode bNode
  {-# INLINABLE fmap #-}

instance Applicative (Node e) where
  pure = MkNodeConst
  {-# INLINE pure #-}
  liftA2 f = MkNode2_ (\_ a b -> f a b)
  {-# INLINABLE liftA2 #-}
  (<*>) = MkNode2_ (\_ f a -> f a)
  {-# INLINABLE (<*>) #-}

instance (Num a) => Num (Node e a) where
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

instance (Fractional a) => Fractional (Node e a) where
  (/) = liftA2 (/)
  {-# INLINABLE (/) #-}
  recip = fmap recip
  fromRational = pure . fromRational

instance (Floating a) => Floating (Node e a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  sqrt = fmap sqrt
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  (**) = liftA2 (**)
  logBase = liftA2 logBase

-- | Make record dot access reach inside a Node
instance (HasField name r a) => HasField name (Node e r) (Node e a) where
  getField = fmap (getField @name)

--------------------------------------------------------------------------------
-- *** Convenient derived constructors

-- | Constructor for a unary node, derived from `MkNode2`.
mkNode1 :: s -> (SigEnv e -> a -> s -> SPair x s) -> Node e a -> Node e x
mkNode1 s sig = MkNode2 s (\r _ a -> sig r a) (pure ())
{-# INLINABLE mkNode1 #-}

-- | Constructor for a unary IO-effectful node, derived from `MkNode2IO`.
mkNode1IO :: s -> (SigEnv e -> a -> s -> IO (SPair x s)) -> Node e a -> Node e x
mkNode1IO s sig = MkNode2IO s (\r _ a -> sig r a) (pure ())
{-# INLINABLE mkNode1IO #-}

-- | Constructor for a ternary node, derived from `MkNode2`. (This pattern of tupling up arg
-- nodes can be extended indefinitely to higher arities if we ever need them.)
mkNode3 :: s -> (SigEnv e -> a -> b -> c -> s -> SPair x s) -> Node e a -> Node e b -> Node e c -> Node e x
mkNode3 s sig ~aNode ~bNode = MkNode2 s (\r (MkS2 a b) -> sig r a b) (liftA2 MkS2 aNode bNode)
{-# INLINABLE mkNode3 #-}

-- | Constructor for a stateless 4-ary node.
mkNode4_ :: (SigEnv e -> a -> b -> c -> d -> x) -> Node e a -> Node e b -> Node e c -> Node e d -> Node e x
mkNode4_ sig ~aNode ~bNode ~cNode ~dNode = MkNode2_ (\r (MkS2 a b) (MkS2 c d) -> sig r a b c d)
  (liftA2 MkS2 aNode bNode) (liftA2 MkS2 cNode dNode)
{-# INLINABLE mkNode4_ #-}

--------------------------------------------------------------------------------
-- ** Observable sharing

data OshCache a = MkOshCached {-# UNPACK #-} !TimeVal !a | MkOshNothing

-- | Uses `unsafePerformIO` to allow a node to cache its output, keyed by current time. If a
-- cache-supporting node is run with a new @t@ /different/ from the @t@ of the current cache
-- value, the cache is cleared & refreshed with a value computed from the signal function.
--
-- This gives /observable sharing/: let-binding a caching node, and using that bound name as
-- the child of other nodes, lets those nodes effectively use the same cached value which is
-- now only ever calculated upon its first use once every new sampling step (i.e. upon every
-- new value of @t@).
--
-- BEWARE: a tree node \"shadowed\" by a never-miss cache lookup due to a copy of itself run
-- before it in the depth-first (postorder) tree walk will /never/ get any state updated, as
-- it is never actually /evaluated/, and state is not part of the value being cached.
--
-- This does not matter, because they nonetheless always yield the right value thanks to the
-- cache, but if it ever becomes possible for the tree to change between time steps & states
-- do not get reset by this change, then we might run into wrong states being used. Also any
-- printing of node tree states will yield the wrong states for these always-shadowed nodes.
--
-- CAVEAT: this has a bit of overhead (`unsafePerformIO` and the `IORef` both incur a cost),
-- though it will be beneficial for most nontrivial nodes and especially ones that are piped
-- into many other nodes as arguments. Do profiling to inform judgment of whether `share` is
-- helping or not, because it can be non-obvious.
--
-- See [reactive-banana's writeup](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/doc/design/design.md)
-- and [implementation](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Prim/High/Cached.hs)
-- of this approach (observable sharing had been described in papers prior.)
share :: Node e x -> Node e x
share me@(MkNodeConst _) = me
share ~node = unsafePerformIO $ newIORef MkOshNothing <&> \ref -> MkNode0IO Nothing $
  \r@(!t, _, _) ~maybeMyNode -> readIORef ref >>= \case
    -- on cache miss, run (either the original node or our saved current version of it) then
    -- cache the result, then return it, and save (Just new_node) as our new saved state. If
    -- the cache hits, then the cache ALWAYS hits, because we come after the 1st instance of
    -- this shared node in the tree, our node state is always Nothing, runit never runs, and
    -- thereby guaranteeing that ONLY 1 copy of the shared node's tree is stored anywhere in
    -- the final tree.
    (MkOshCached tc xc) | tc == t -> pure (MkS2 xc maybeMyNode)
    _ -> runNode r (maybe node id maybeMyNode) >>= \(MkS2 x new_node) ->
      writeIORef ref (MkOshCached t x) *> pure (MkS2 x (Just new_node))
{-# NOINLINE share #-}

--------------------------------------------------------------------------------
-- ** Consuming nodes at the audio backend

-- | Run the audio node tree one step, given a signal environment. Returns the emitted value
-- and an updated tree (with the new node states as returned by the signal functions).
runNode :: SigEnv e -> Node e x -> IO (SPair x (Node e x))
runNode r node = case node of
  MkNodeConst x -> pure $ MkS2 x node
  MkNode0_ sig  -> pure $ MkS2 (sig r) node
  MkNode0 s sig -> let
    (MkS2 x new_s) = sig r s
    in pure $ MkS2 x (MkNode0 new_s sig)
  MkNode0IO s sig -> do
    (MkS2 x new_s) <- sig r s
    pure $ MkS2 x (MkNode0IO new_s sig)
  MkNode2 s sig aNode bNode -> do
    (MkS2 a new_aNode) <- runNode r aNode
    (MkS2 b new_bNode) <- runNode r bNode
    let (MkS2 x new_s) = sig r a b s
    pure $ MkS2 x (MkNode2 new_s sig new_aNode new_bNode)
  MkNode2_ sig aNode bNode -> do
    (MkS2 a new_aNode) <- runNode r aNode
    (MkS2 b new_bNode) <- runNode r bNode
    let x = sig r a b
    pure $ MkS2 x (MkNode2_ sig new_aNode new_bNode)
  MkNode2IO s sig aNode bNode -> do
    (MkS2 a new_aNode) <- runNode r aNode
    (MkS2 b new_bNode) <- runNode r bNode
    (MkS2 x new_s) <- sig r a b s
    pure $ MkS2 x (MkNode2IO new_s sig new_aNode new_bNode)

-- | Queries a node @n@ times given the signal environment and a `SigEnv`-stepping function.
-- This gives a very simple way to sample the node tree and produce an array of samples, but
-- a list probably won't be very fast for downstream uses.
iterateNode :: SigEnv e -> (SigEnv e -> SigEnv e) -> Int -> Node e x -> IO (SPair [x] (Node e x))
iterateNode r stepper n node = if n <= 0 then pure $ MkS2 [] node else do
  (MkS2 x nodeNext) <- runNode r node
  (MkS2 xs nodeFinal) <- iterateNode (stepper r) stepper (pred n) nodeNext
  pure $ MkS2 (x : xs) nodeFinal

-- | A convenience state type for an audio backend to save. The audio backend is responsible
-- for consuming @_destNode@ and stepping/updating the @_sigEnv@ (to new time values.)
data Sink e =
  MkSink  { _destNode :: !(Node e (LR SampleVal))
          , _sigEnv :: !(TimeVal, TimeVal, e)
          }

-- | Initializing a `Sink` with a pure zero `Node`.
initSink :: SampleRate -> e -> Sink e
initSink sampleRate e =
  MkSink  { _destNode = pure zeroLR
          , _sigEnv = (0.0, recip $ fromIntegral sampleRate, e)
          }

--------------------------------------------------------------------------------
-- ** Basic operators

-- | Generic accumulator node, with a starting state and an accumulating function. The state
-- updates every time a value is emitted; the emitted value is that state.
accum :: s -> (i -> s -> s) -> Node e i -> Node e s
accum sta accumFn = mkNode1 sta $ \_ i s -> let s' = accumFn i s in MkS2 s' s'

-- | Simple counter with a starting count, accumulating new values received from a node.
tick :: Num a => a -> Node e a -> Node e a
tick sta = mkNode1 sta $ \_ i s -> MkS2 s (s + i)

-- test stuff
-- __testenv = (0.0 :: TimeVal, 1.0 :: TimeVal)

-- __stepper :: SigEnv -> SigEnv
-- __stepper = (\(t, d) -> (t + d, d))

-- __testApplic = let ticker = cache $ tick 1 1 in iterateNode __testenv __stepper 3 $ (\a b c -> a + b + c) <$> ticker <*> ticker <*> ticker
-- __testPiecewise = iterateNode __testenv __stepper 14 $
--   startAt (-1) 2.0 (Just 5) $ piecewise 1.0 0 [(tick 10 1, 3), (tick 20 1, 2), (tick 30 1, 4)]

-- | Retrieve the time tick context from the signal environment, as a node.
nTime :: Node e TimeVal
nTime = MkNode0_ $ \(!t, _, _) -> t
{-# INLINABLE nTime #-}

-- | Mix two nodes with addition.
(|+|) :: (Num a) => Node e a -> Node e a -> Node e a
{-# SPECIALIZE (|+|) :: Node e SynthVal -> Node e SynthVal -> Node e SynthVal #-}
{-# SPECIALIZE (|+|) :: Node e (LR SynthVal) -> Node e (LR SynthVal) -> Node e (LR SynthVal) #-}
(|+|) = liftA2 (+)
{-# INLINABLE (|+|) #-}
infixl 3 |+|

-- | Since we have `|+|`, we might as well have `|*|` which multiplies two nodes pointwise.
(|*|) :: (Num a) => Node e a -> Node e a -> Node e a
{-# SPECIALIZE (|*|) :: Node e SynthVal -> Node e SynthVal -> Node e SynthVal #-}
{-# SPECIALIZE (|*|) :: Node e (LR SynthVal) -> Node e (LR SynthVal) -> Node e (LR SynthVal) #-}
(|*|) = liftA2 (*)
{-# INLINABLE (|*|) #-}
infixl 4 |*|

-- | Add to a node a scalar addend.
(+|) :: (Num a) => a -> Node e a -> Node e a
{-# SPECIALIZE (+|) :: SynthVal -> Node e SynthVal -> Node e SynthVal #-}
(+|) addend = fmap (addend +)
infixr 7 +|
{-# INLINABLE (+|) #-}

-- | Add to a stereo node a scalar addend.
(+||) :: SynthVal -> Node e (LR SynthVal) -> Node e (LR SynthVal)
-- (+||) :: (LRxNum a) => a -> Node e (LR a) -> Node e (LR a)
-- {-# SPECIALIZE (+||) :: SynthVal -> Node e (LR SynthVal) -> Node e (LR SynthVal) #-}
-- LRxNum is too internal to Sound.Hailstone.Types.LR, we shouldn't be using it out here...
-- better to just specialize it (as we've given (LR T) types specialized reprs anyway)
(+||) addend = fmap (addend .+:)
infixr 7 +||
{-# INLINABLE (+||) #-}

-- | Scale a node by a scalar multiplier.
(*|) :: (Num a) => a -> Node e a -> Node e a
{-# SPECIALIZE (*|) :: SynthVal -> Node e SynthVal -> Node e SynthVal #-}
(*|) multiplier = fmap (multiplier *)
infixr 8 *|
{-# INLINABLE (*|) #-}

-- | Scale a stereo node by a scalar multiplier.
(*||) :: SynthVal -> Node e (LR SynthVal) -> Node e (LR SynthVal)
-- (*||) :: (LRxNum a) => a -> Node e (LR a) -> Node e (LR a)
-- {-# SPECIALIZE (*||) :: SynthVal -> Node e (LR SynthVal) -> Node e (LR SynthVal) #-}
(*||) multiplier = fmap (multiplier .*:)
infixr 8 *||
{-# INLINABLE (*||) #-}

-- | Warp the time axis for a node using a function that modifies time AND its derivative to
-- modify deltaTime. (If only we could find the derivative automatically?)
modifyTime :: (TimeVal -> TimeVal) -> (TimeVal -> TimeVal) -> Node e a -> Node e a
modifyTime f df node = case node of
  (MkNodeConst _) -> node
  (MkNode0 s sig) -> MkNode0 s (sfgo sig)
  (MkNode0_ sig) -> MkNode0_ (sfgo sig)
  (MkNode0IO s sig) -> MkNode0IO s (sfgo sig)
  -- must recursively modify time for the child nodes too since our signal function concerns
  -- only ourselves; the child nodes are evaluated before we are.
  -- TODO a bool on child nodes to enable/disable modifyTime recurse?
  (MkNode2 s sig aNode bNode) -> MkNode2 s (sfgo sig) (modifyTime f df aNode) (modifyTime f df bNode)
  (MkNode2_ sig aNode bNode) -> MkNode2_ (sfgo sig) (modifyTime f df aNode) (modifyTime f df bNode)
  (MkNode2IO s sig aNode bNode) -> MkNode2IO s (sfgo sig) (modifyTime f df aNode) (modifyTime f df bNode)
  where
    sfgo :: (SigEnv e -> a) -> SigEnv e -> a
    sfgo sig (!t, d, e) = sig (f t, df d, e)

-- | Have a node start emitting at a start time (and possibly end after some duration), else
-- emitting a default empty value outside the allowed timespan.
startAt :: a -> TimeVal -> TimeVal -> Node e a -> Node e a
startAt empt sta du ~node' = let s = modifyTime (subtract sta) id node'
  in MkNode0IO s $ \r@(!t, _, _) node -> if
    | t < sta -> pure $ MkS2 empt node
    | t < (sta + du) -> runNode r node
    | otherwise -> pure $ MkS2 empt node

--------------------------------------------------------------------------------
-- ** Numeric and stereo conversions

-- | Round and clip a node's output to the bounds allowed by the sample format.
roundClip :: Node e (LR SynthVal) -> Node e (LR SampleVal)
-- roundClip :: (LRxNum v, RealFrac v, LRx samp, Bounded samp, Ord samp, Integral samp) => Node e (LR v) -> Node e (LR samp)
-- {-# SPECIALIZE roundClip :: Node e (LR SynthVal) -> Node e (LR SampleVal) #-}
roundClip = fmap $ mapLR $ \v -> round
  $ min (fromIntegral (maxBound :: SampleVal))
  $ max (fromIntegral (minBound :: SampleVal)) v

-- | Convert a double-generating signal into a signal of sample values expected by our audio
-- setup (here, @Int16@), with scaling up to sample values followed by rounding and clipping
asPCM :: Node e (LR SynthVal) -> Node e (LR SampleVal)
-- asPCM :: (LRxNum v, RealFrac v) => Node e (LR v) -> Node e (LR SampleVal)
-- {-# SPECIALIZE asPCM :: Node e (LR SynthVal) -> Node e (LR SampleVal) #-}
asPCM = roundClip . (*|) (fromIntegral (maxBound :: SampleVal))

sqrt2over2 :: SynthVal
sqrt2over2 = 0.5 * sqrt 2

-- | Turn a mono real-valued signal into a center-panned stereo signal.
-- (using constant power pan law)
mono2stereo :: Node e SynthVal -> Node e (LR SynthVal)
mono2stereo = fmap (\a -> dupLR $ sqrt2over2 * a)
{-# INLINABLE mono2stereo #-}

stereo2mono :: Node e (LR SynthVal) -> Node e SynthVal
stereo2mono = fmap (\a -> sqrt2over2 * hsumLR a)
{-# INLINABLE stereo2mono #-}

-- | Duplicate a mono real-valued signal into a stereo signal without pan scale adjustment
stereodup :: Node e SynthVal -> Node e (LR SynthVal)
stereodup = fmap dupLR
{-# INLINABLE stereodup #-}

-- | Sum left and right of a mono to trivially get a mono signal, without level adjustment.
monosum :: Node e (LR SynthVal) -> Node e SynthVal
monosum = fmap hsumLR
{-# INLINABLE monosum #-}

-- | Lift a function on mono nodes to become a function on stereo nodes
mapLRNode :: (Node e SynthVal -> Node e SynthVal) -> Node e (LR SynthVal) -> Node e (LR SynthVal)
mapLRNode f lrNode = let
  lrNode' = share lrNode
  lNode = f $ fmap (\lr -> withLR lr $ \l _ -> l) lrNode'
  rNode = f $ fmap (\lr -> withLR lr $ \_ r -> r) lrNode'
  in liftA2 mkLR lNode rNode

-- halfpi :: SynthVal
-- halfpi = 0.5 * pi
-- ! Turn a mono real-valued signal into a panned stereo signal.
-- (Taxing! Use `mono2stereo` followed by `balance` instead.)
-- mono2pan :: Pan -> Node e SynthVal -> Node e (LR SynthVal)
-- mono2pan p = fmap (\a -> mkLR (a * cos (p * halfpi)) (a * sin (p * halfpi)))
-- {-# INLINABLE mono2pan #-}

-- | Applies a different pan value to rebalance a stereo node.
balance :: Pan -> Node e (LR SynthVal) -> Node e (LR SynthVal)
-- balance :: (LRxFrac a, Ord a) => a -> Node e (LR a) -> Node e (LR a)
-- {-# SPECIALIZE balance :: Pan -> Node e (LR SynthVal) -> Node e (LR SynthVal) #-}
balance = fmap . balanceLR
{-# INLINABLE balance #-}

--------------------------------------------------------------------------------
-- ** Sequencing

-- | Sequence nodes together monophonically, given their start times and durations.
piecewiseMono :: TimeVal
              -- ^the start time for this sequence of nodes
              -> a -- ^empty value to emit when no nodes are scheduled to be playing
              -> [(Node e a, TimeVal, TimeVal)]
              -- ^a list of @[(node, start, duration)]@, __assumed to already be sorted by
              -- start time!!__ The node starts at @start@ and plays til the next node with
              -- the next start time. @duration@ is ignored except for the last node.
              -> Node e a
piecewiseMono t0 empt nodesDurs = out
  where
  -- assume sorted by start time, apply (t0+start) offsets to everyone
  nodesDurs' = fmap (\(node, sta, du) -> let sta' = sta + t0
    in (startAt empt sta' du node, sta', du)) $ nodesDurs
  -- get initial state and form the output node
  out = case nodesDurs' of
    [] -> pure empt
    ((firstNode, !firstStart, !firstDur):restNodes) -> MkNode0IO
      (firstNode, firstStart, firstDur, restNodes) gogo
  -- the state function for the node. check the start time of the next node and switch to it
  -- and consume one node from the list if its time is here, otherwise just keep playing the
  -- current node
  gogo r@(t, _, _) (currNode, !currStart, !currDur, nil@[]) = do
    (MkS2 x new_currNode) <- if t < currStart + currDur then runNode r currNode else pure $ MkS2 empt currNode
    pure $ MkS2 x (new_currNode, currStart, currDur, nil)
  gogo r@(t, _, _) s@(currNode, currStart, currDur, ls@((nextNode, !nextStart, nextDur):rest))
    | t < currStart = pure $ MkS2 empt s
    | t < nextStart = runNode r currNode <&> \(MkS2 x new_currNode) -> MkS2 x (new_currNode, currStart, currDur, ls)
    | otherwise = nextStart `seq` nextDur `seq` rest `seq`
      gogo r (nextNode, nextStart, nextDur, rest)

-- | Sequence nodes together polyphonically (i.e. with many voices so that nodes overlapping
-- is allowed), given their start times and durations. Uses voice allocation to minimize the
-- number of nodes playing at once. (There's no actual concept of a voice here, but this way
-- /does/ lead to reduced memory usage and GC because there are fewer nodes happening all at
-- once being summed up. See `cascade` for more naive and inefficient polyphonic sequencing.
piecewisePoly :: Num a
              => TimeVal
              -- ^the start time for this sequence of nodes
              -> a -- ^empty value to emit when no nodes are scheduled to be playing
              -> [(Node e a, TimeVal, TimeVal)]
              -- ^a list of @[(node, start, duration)]@.
              -> Node e a
{-# SPECIALIZE piecewisePoly :: TimeVal -> LR SynthVal -> [(Node e (LR SynthVal), TimeVal, TimeVal)] -> Node e (LR SynthVal) #-}
piecewisePoly t0 empt = sum . fmap (piecewiseMono t0 empt) . VoiceAlloc.voiceAlloc

-- | A far less efficient way to do `piecewisePoly`, just summing up all the nodes once they
-- have been individually shifted on the timeline. This implies all nodes are always playing
-- which can get expensive when e.g. each node is one note.
cascade :: (Foldable f, Functor f, Num a)
        => TimeVal -- ^the start time for this sequence of nodes
        -> a -- ^empty value to emit when no nodes are scheduled to be playing
        -> f (Node e a, TimeVal, TimeVal) -- ^a list of @[(node, start, duration)]@
        -> Node e a
cascade t0 empt = sum . fmap (\(node, sta, du) -> startAt empt (t0 + sta) du node)

--------------------------------------------------------------------------------
-- ** Envelopes

-- | Pure function for `funcRamp`
_funcRamp_fn :: (Num v, Ord v) => (TimeVal -> v) -> TimeVal -> v -> v -> TimeVal -> v
{-# SPECIALIZE _funcRamp_fn :: (TimeVal -> SynthVal) -> TimeVal -> SynthVal -> SynthVal -> TimeVal -> SynthVal #-}
_funcRamp_fn f du sta end t = let
  downramp = sta > end
  MkS2 smaller bigger = if downramp then MkS2 end sta else MkS2 sta end
  up = clamp (smaller, bigger) $ smaller + (bigger - smaller) * (f $ t / du)
  in if downramp then smaller + bigger - up else up

-- | A ramp signal that increases from start to end (or possibly decreases) in a given range
-- of time based on some given function, then holds. For linear ramping, the function should
-- be @\t -> t@; For a quadratic ramp, the function is @\t -> t*t@ etc.
funcRamp :: (Num v, Ord v) => (TimeVal -> v) -> TimeVal -> v -> v -> Node e v
funcRamp f du sta end = if du == 0 then pure end else nTime <&> _funcRamp_fn f du sta end
{-# INLINABLE funcRamp #-}
{-# SPECIALIZE funcRamp :: (TimeVal -> SynthVal) -> TimeVal -> SynthVal -> SynthVal -> Node e SynthVal #-}

-- | Specialization of funcRamp to a linear function
linearRamp :: TimeVal -> SynthVal -> SynthVal -> Node e SynthVal
linearRamp = funcRamp id

-- | Create a signal traversing an ADSR envelope according to some fixed envelope params.
adsr' :: ADSRParams -> Node e Percent
adsr' (ADSR tA tD tS tR v0 v1 v2) = let
  dA = (1.0 - v0) / tA
  dD = (v1 - 1.0) / tD
  t0S = tA + tD
  t0R = t0S + tS
  dR = (v2 - v1) / tR
  tEnd = t0R + tR
  sig (!t, _, _)
    | t < tA = v0 + t * dA
    | t < t0S = 1.0 + (t - tA) * dD
    | t < t0R = v1
    | t < tEnd = v1 + (t - t0R) * dR
    | otherwise = v2
  in share $ MkNode0_ sig

-- | Convenience shorthand for `adsr'` on an `ADSR` literal.
adsr :: TimeVal -> TimeVal -> TimeVal -> TimeVal -> Percent -> Percent -> Percent -> Node e Percent
adsr tA tD tS tR v0 v1 v2 = adsr' $ ADSR tA tD tS tR v0 v1 v2
{-# INLINABLE adsr #-}

-- | @NaN@ literal
nan :: SynthVal
nan = 0 / 0

-- | the Prelude @isNaN@ goes through some unsafe IO primop, this is probably faster
isNaN :: SynthVal -> Bool
isNaN a = a /= a

-- | A node-parameterized version of `adsr'` which can take `ADSRParams` emitted from
-- a node. Note that this works fairly differently from `adsr'`: ADSR parameters here
-- are variable so we have to keep the envelope state, incrementing it with a slope computed
-- using the latest received ADSR parameters.
adsr'N :: Node e ADSRParams -> Node e Percent
adsr'N = mkNode1 (MkS2 nan Nothing) $
  \(!t, d, _) (ADSR tA tD tS tR v0 v1 v2) (MkS2 x' precalcs') -> let
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
    newS = MkS2 newX (case precalcs' of Nothing -> Just precalcs; _ -> precalcs')
    -- clamp output to be between 0 and 1
    in MkS2 newX newS

--------------------------------------------------------------------------------
-- ** Generators

twopi :: SynthVal
twopi = 2 * pi

-- | generic oscillator function with \"phase\" accumulator (but \"phase\" is from 0 to 1)
_osc_fn :: TimeVal -> Freq -> Percent -> (Percent -> SynthVal) -> SPair SynthVal Percent
_osc_fn d f myProgress k = let
  progressDelta = f * d
  newProgress' = myProgress + progressDelta
  newProgress = if newProgress' > 1 then newProgress' - 1 else newProgress'
  in MkS2 (k newProgress) newProgress
  -- progress is from 0 to 1 (assuming f is never negative to not have to check <0...)
{-# INLINE _osc_fn #-}

_osc_s0 :: Percent
_osc_s0 = 0.0
{-# INLINE _osc_s0 #-}

---------- sin oscillator with adjustable phase addition
_sinOsc_fn :: TimeVal -> Ampl -> Freq -> Phase -> Percent -> SPair SynthVal Percent
_sinOsc_fn d a f phase s = _osc_fn d f s $ \x -> a * sin (twopi * x + phase)

_sinOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_sinOsc_SF (_, d, _) a f = _sinOsc_fn d a f 0

_sinOscPM_SF :: SigEnv e -> Ampl -> Freq -> Phase -> Percent -> SPair SynthVal Percent
_sinOscPM_SF (_, d, _) a f phase = _sinOsc_fn d a f phase

-- | Sine oscillator taking amplitude and frequency as input nodes.
sinOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sinOsc = MkNode2 _osc_s0 _sinOsc_SF

-- | `sinOsc` with phase modulation as a third input node.
sinOscPM :: Node e Ampl -> Node e Freq -> Node e Phase -> Node e SynthVal
sinOscPM = mkNode3 _osc_s0 _sinOscPM_SF

---------- square oscillator with adjustable duty cycle
_sqrOsc_fn :: TimeVal -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sqrOsc_fn d a f duty s = _osc_fn d f s $ \x -> if x <= duty then (-a) else a

_sqrOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_sqrOsc_SF (_, d, _) a f = _sqrOsc_fn d a f 0.5

_sqrOscDM_SF :: SigEnv e -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sqrOscDM_SF (_, d, _) a f duty = _sqrOsc_fn d a f duty

-- | Square oscillator taking amplitude and frequency as input nodes.
sqrOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sqrOsc = MkNode2 _osc_s0 _sqrOsc_SF

-- | `sqrOsc` with duty cycle as a third input node.
sqrOscDM :: Node e Ampl -> Node e Freq -> Node e Percent -> Node e SynthVal
sqrOscDM = mkNode3 _osc_s0 _sqrOscDM_SF

---------- saw oscillator with adjustable skew (0.5 = triangle, 0.0 or 1.0 = saw)
_saw_fn :: Ampl -> Percent -> Percent -> SynthVal
_saw_fn a skew x = let y = if x < skew then (x / skew) else (1 - x) / (1 - skew)
  in a * (2 * y - 1)

_sawOsc_fn :: TimeVal -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sawOsc_fn d a f skew s = _osc_fn d f s $ _saw_fn a skew

_sawOscSM_SF :: SigEnv e -> Ampl -> Freq -> Percent -> Percent -> SPair SynthVal Percent
_sawOscSM_SF (_, d, _) a f = _sawOsc_fn d a f

_sawOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_sawOsc_SF (_, d, _) a f = _sawOsc_fn d a f 1.0

_triOsc_SF :: SigEnv e -> Ampl -> Freq -> Percent -> SPair SynthVal Percent
_triOsc_SF (_, d, _) a f = _sawOsc_fn d a f 0.5

-- | Saw oscillator taking amplitude and frequency as input nodes.
sawOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sawOsc = MkNode2 _osc_s0 _sawOsc_SF

-- | Triangle oscillator taking amplitude and frequency as input nodes.
triOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
triOsc = MkNode2 _osc_s0 _triOsc_SF

-- | `sawOsc` with skew as a third input node. (0 and 1 = saw, 0.5 = triangle)
sawOscSM :: Node e Ampl -> Node e Freq -> Node e Percent -> Node e SynthVal
sawOscSM = mkNode3 _osc_s0 _sawOscSM_SF

-- | `triOsc` with \"phase modulation\", almost like a \"coarser\", tri-shaped `sinOscPM`.
triOscPM :: Node e Ampl -> Node e Freq -> Node e Phase -> Node e SynthVal
triOscPM = let r2pi = recip twopi in mkNode3 _osc_s0 $ \(_, d, _) a f phase s ->
  _osc_fn d f s $ \x -> _saw_fn a 0.5 (x + phase * r2pi)

--------------------------------------------------------------------------------
-- ** Effects
--
-- Effects must be either polymorphic or at least be specialized to @`LR` `SynthVal`@ rather
-- than just SynthVal.

-- | Convert a gain value (in decibels) to amplitude, equal to @10**(dBgain/20)@.
gain2ampl :: Floating a => a -> a
gain2ampl g = 10.0 ** (0.05 * g)
{-# INLINABLE gain2ampl #-}

-- | `gain2ampl` specialized for nodes, spawning fewer nodes.
nGain2ampl :: Floating a => Node e a -> Node e a
nGain2ampl g = fmap (10.0 **) (0.05 *| g)
{-# INLINABLE nGain2ampl #-}

-- | Apply constant gain (given in dB) to a node.
gain :: Floating a => a -> Node e a -> Node e a
gain g ~node = let a = gain2ampl g in a *| node
{-# INLINABLE gain #-}

-- | Apply variable gain (given in dB, emitted from a node) to a node.
gainN :: Floating a => Node e a -> Node e a -> Node e a
gainN g ~node = let a = nGain2ampl g in a * node
{-# INLINABLE gainN #-}

-- *** Filters à la RBJ's EQ cookbook

_cookbookFilter_fn :: CookbookFilterCoeffs -> CookbookFilterState -> LR SynthVal -> CookbookFilterState
_cookbookFilter_fn (MkCFCoeffs a0inv a1 a2 b0 b1 b2) (MkCFState xm1 xm2 _ ym1 ym2 _) x = let
  y = a0inv .*: ((b0 .*: x) + (b1 .*: xm1) + (b2 .*: xm2) - (a1 .*: ym1) - (a2 .*: ym2))
  in MkCFState x xm1 xm2 y ym1 ym2

_cookbookFilter_nanCoeffs :: CookbookFilterCoeffs
_cookbookFilter_nanCoeffs = MkCFCoeffs nan nan nan nan nan nan

_cookbookFilter_zeroState :: CookbookFilterState
_cookbookFilter_zeroState = MkCFState 0 0 0 0 0 0

_cookbookFilter_s0 :: SPair CookbookFilterCoeffs CookbookFilterState
_cookbookFilter_s0 = MkS2 _cookbookFilter_nanCoeffs _cookbookFilter_zeroState

_cookbookFilterN_s0 :: STriple Freq CookbookFilterCoeffs CookbookFilterState
_cookbookFilterN_s0 = MkS3 nan _cookbookFilter_nanCoeffs _cookbookFilter_zeroState

_cookbookFilter_SF :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> Freq -> SigEnv e -> LR SynthVal -> SPair CookbookFilterCoeffs CookbookFilterState -> SPair (LR SynthVal) (SPair CookbookFilterCoeffs CookbookFilterState)
_cookbookFilter_SF coeffsFn q f (_, d, _) x (MkS2 savedCoeffs savedState) = let
  c = if isNaN savedCoeffs.a0inv then coeffsFn d q f else savedCoeffs
  s = _cookbookFilter_fn c savedState x
  in MkS2 s.ym0 (MkS2 c s)

_cookbookFilterN_SF :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> SigEnv e -> Freq -> LR SynthVal -> STriple Freq CookbookFilterCoeffs CookbookFilterState -> SPair (LR SynthVal) (STriple Freq CookbookFilterCoeffs CookbookFilterState)
_cookbookFilterN_SF coeffsFn q (_, d, _) thisFreq x (MkS3 lastFreq savedCoeffs savedState) = let
  c = if thisFreq == lastFreq then savedCoeffs else coeffsFn d q thisFreq
  s = _cookbookFilter_fn c savedState x
  in MkS2 s.ym0 (MkS3 thisFreq c s)

cookbookCoeffsFn_LPF :: TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs
cookbookCoeffsFn_LPF d q f = let
  omega = twopi * f * d
  cosom = cos omega
  sinom = sin omega
  alpha = sinom / (2 * q)
  b0 = (1 - cosom) / 2
  in MkCFCoeffs
    { a0inv = recip (1 + alpha)
    , a1 = -2 * cosom
    , a2 = 1 - alpha
    , b0 = b0
    , b1 = 1 - cosom
    , b2 = b0
    }

-- | RBJ 2nd-order filter. <https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html>
-- Takes a coefficients-calculating function (which itself takes delta time, Q, frequency.)
cookbookFilter :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
cookbookFilter coeffsFn q f = mkNode1 _cookbookFilter_s0 (_cookbookFilter_SF coeffsFn q f)

-- | `cookbookFilter` but with variable filter frequency from a node.
cookbookFilterN :: (TimeVal -> SynthVal -> Freq -> CookbookFilterCoeffs) -> SynthVal -> Node e Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
cookbookFilterN coeffsFn q = MkNode2 _cookbookFilterN_s0 (_cookbookFilterN_SF coeffsFn q)

-- | 2nd-order low-pass filter.
lpf :: SynthVal -> Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
lpf = cookbookFilter cookbookCoeffsFn_LPF

-- | `lpf` but with variable filter frequency.
lpfN :: SynthVal -> Node e Freq -> Node e (LR SynthVal) -> Node e (LR SynthVal)
lpfN = cookbookFilterN cookbookCoeffsFn_LPF

-- *** Delay, echo

-- | Single-stream echo with constant delay time, decay multiplier, wet mix level (0-1), low
-- pass filter Q factor, cutoff frequency, and pan applied to the wet signal.
--
-- (More efficient, fused implementation of `echo`.)
echo' :: TimeVal -> Ampl -> Ampl -> SynthVal -> Freq -> Pan -> Node e (LR SynthVal) -> Node e (LR SynthVal)
echo' delayMs decayMult wetLvl filterQ filterF wetPan = let delaySec = delayMs * 0.001; dryLvl = max 0 (1 - wetLvl)
  in mkNode1IO (MkS2 DelayQueue.delay_s0 _cookbookFilter_s0) $ \r@(_, d, _) x (MkS2 myDQ filterState) ->
    DelayQueue.withDelay delaySec decayMult d myDQ x $ \dq delayOut _ -> let
      (MkS2 filteredDelayOut filterState') = _cookbookFilter_SF
        cookbookCoeffsFn_LPF filterQ filterF r delayOut filterState
      in pure $ MkS2 ((dryLvl .*: x) + balanceLR wetPan (wetLvl .*: filteredDelayOut))
        (MkS2 dq filterState')

-- | Single-stream echo with constant delay time and decay multiplier. This produces the raw
-- echo wet signal; do your own mixing and filtering afterwards (or use `echo`.)
echoRaw :: TimeVal -> Ampl -> Node e (LR SynthVal) -> Node e (LR SynthVal)
echoRaw delayMs decayMult = let delaySec = delayMs * 0.001
  in mkNode1IO DelayQueue.delay_s0 $ \(_, d, _) x myDQ ->
    DelayQueue.withDelay delaySec decayMult d myDQ x $ \dq delayOut _ -> pure $ MkS2 delayOut dq

-- | Single-stream echo with constant delay time, decay multiplier, wet mix level (0-1), low
-- pass filter Q factor, cutoff frequency, and pan applied to the wet signal.
--
-- Less efficient but more readable and extensible implementation of `echo'`.
echo :: TimeVal -> Ampl -> Ampl -> SynthVal -> Freq -> Pan -> Node e (LR SynthVal) -> Node e (LR SynthVal)
echo delayMs decayMult wetLvl filterQ filterF wetPan input = output
  where
    input' = share input
    filteredDelayOut = lpf filterQ filterF $ echoRaw delayMs decayMult input'
    output = (1 - wetLvl) *|| input' + (balance wetPan $ wetLvl *|| filteredDelayOut)
