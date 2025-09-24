{-# LANGUAGE GADTs, BangPatterns #-}

module Sound.Hailstone.Synth
( -- * Type synonyms and datatypes
  -- *** Re-exports from "Sound.Hailstone.Types"
  SynthVal, Freq, Vol, Pan, SampleRate, TimeVal, SampleVal
, ChanMode(..)
, Cell(..)
  -- *** Convenience tuple type pairing a left and right sample, for stereo audio
, LR(..)
  -- * Audio node tree. An audio `Node` is a unit of computation with a state, and carries a
  -- signal function that reads and updates the state and emits a value. A `Node` can parent
  -- child nodes (whose emitted values feed into the parent), forming a tree of nodes. Check
  -- the constructors for the minimal set of axiomatic nodes that cover our evaluation needs
  -- and see `mkNode1` and `mkNode3` for some derived constructors.
, Node(..)
, mkNode1, mkNode3
  -- ** For consuming nodes at the audio backend
, Sink(..), initSink, runNode, iterateNode
  -- ** Simple generators
, sinOsc, sinOscP
  -- ** Combinators and operations
, (|+|), (|*|), (+|), (*|), accum, tick
  -- ** Converting to numeric types and stereo pairs used by the audio backend
, roundClip, asPCM, stereoize', stereoize, m2s
  -- ** Sequencing nodes
, startAt, piecewise, cascade
, retriggerWithNotes
  -- ** Envelopes
, ADSRParams(..)
, funcRamp, linearRamp, adsrEnvelope
)
where

import Data.Functor ((<&>))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Sound.Hailstone.Types

-- | A signal function will need access to the current time, as well as the time
-- delta between samplings (i.e. 1 / (samplerate))
type SigEnv = (TimeVal, TimeVal)

-- | Convenience tuple of two audio values, one for each channel (left and right).
newtype LR a = MkLR { lrTup :: (a, a) }
  deriving (Eq, Show)

instance Functor LR where
  fmap f (MkLR (al, ar)) = MkLR (f al, f ar)

instance Applicative LR where
  pure = dupLR
  (MkLR (fl, fr)) <*> (MkLR (al, ar)) = MkLR (fl al, fr ar)

-- | Trivially make a mono value into a stereo value.
dupLR :: a -> LR a
dupLR a = MkLR (a, a)
{-# INLINABLE dupLR #-}

instance (Num a) => Num (LR a) where
  (MkLR (al, ar)) + (MkLR (bl, br)) = MkLR ((al + bl), (ar + br))
  (MkLR (al, ar)) * (MkLR (bl, br)) = MkLR ((al * bl), (ar * br))
  (MkLR (al, ar)) - (MkLR (bl, br)) = MkLR ((al - bl), (ar - br))
  abs (MkLR (al, ar)) = MkLR ((abs al), (abs ar))
  negate (MkLR (al, ar)) = MkLR ((negate al), (negate ar))
  signum (MkLR (al, ar)) = MkLR ((signum al), (signum ar))
  fromInteger = dupLR . fromInteger

instance (Fractional a) => Fractional (LR a) where
  (MkLR (al, ar)) / (MkLR (bl, br)) = MkLR ((al / bl), (ar / br))
  recip (MkLR (al, ar)) = MkLR ((recip al), (recip ar))
  fromRational = dupLR . fromRational

-- | Signal function: a signal function reads the signal environment @r@ (e.g., to get time,
-- delta time) in addition to an argument @a@  (whose type depends on the arity of the audio
-- node), emits a value @x@, and updates the state @s@.
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
- Q: Why is `MkNodeC` so weird?
    - A: To implement `startAt`, which pauses nodes until a certain time range. Without some
      notion of "conditional execution" (and as we don't have a monad instance---unless I am
      just dense and wasn't able to come up with a legitimate implementation), we require an
      axiomatic introduction of that ability, i.e. a dedicated constructor that embodies the
      concept of conditional execution of a child node upon evaluation. (One potential trick
      to reduce the number of constructors and lump `MkNode2` into `MkNodeC` is to give it a
      third node argument, so that MkNodeC takes /two/ strictly-evaluated argument nodes and
      not just the one @Node a@. But then writing @(<*>)@ would be really hard because it'll
      involve reaching into fNode somehow to run its @sig@ and grab the resultant states and
      not just the resultant Node like you might get by calling runNode; this is bad because
      @s@ is existentially-quantified, you cannot pattern match into @fNode@ like this.)
-}
data Node x where
  MkNode0 :: (Show s) => !s -> !(SF () s x) -> Node x
  -- ^nullary node, representing a source of values with no arguments needed.
  MkNode2 :: (Show s) => !s -> !(SF (f, a) s x) -> !(Node f) -> !(Node a) -> Node x
  -- ^binary node. can represent any time-varying computation taking two argument nodes, but
  -- can also be used to lift, into a node, via the applicative instance, the application of
  -- a pure function @f@ to a pure argument @a@.
  MkNodeC :: (Show s) => !s -> !(SF a s (Either b x)) -> !(Node a) -> !(Node (b -> x)) -> Node x
  -- ^"conditional" node, modeling conditional eval of an input node (because on running the
  -- node tree, `MkNode2` strictly evaluates both child nodes unconditionally before it gets
  -- evaluated itself.) In particular, when evaluating  @(MkNodeC s sig aNode fNode)@, we'll
  -- immediately execute @aNode :: (`Node` a)@ to get a value @a@, then using @s@ & @a@, the
  -- signal function @sig :: `SF` a s (Either b x)@ either
  -- * outputs @Right defaultX :: Either b x@ which will be emitted as the output right away
  --    *without* running the child node @fNode :: (`Node` (b -> x))@, __or__
  -- * outputs @Left realB :: Either b x@, in which case we go on and run the node @fNode ::
  --    (`Node` (b -> x))@ to get and run that function on realB to then emit an output @x@.

instance Functor Node where
  fmap f (MkNode0 !s !sig) = MkNode0 s (fmap f sig)
  fmap f (MkNode2 !s !sig !aNode !bNode) = MkNode2 s (fmap f sig) aNode bNode
  fmap f (MkNodeC !s !sig !aNode !fNode) = MkNodeC s (fmap (fmap f) sig) aNode (fmap (fmap f) fNode)

instance Applicative Node where
  pure = MkNode0 () . pure
  {-# INLINABLE pure #-}
  !fNode <*> !xNode = MkNode2 () (sfArgs >>= \(!f, !x) -> pure (f x)) fNode xNode
  {-# INLINABLE (<*>) #-}
  -- this case ^ is general enough to do everything, and profiling shows it is actually more
  -- allocation efficient and faster than going through the below special cases, even though
  -- they don't appear to increase the number of nodes at play, or even actually decreasing.

  -- (MkNode0 sF sigF) <*> (MkNode0 sX sigX) = MkNode0 (sF, sX) (sfr $ \r _ (sF', sX') -> let
  --   (f, new_sF) = runSF sigF r () sF'
  --   (x, new_sX) = runSF sigX r () sX'
  --   in (f x, (new_sF, new_sX)))
  -- (MkNode0 sF sigF) <*> (MkNode2 sX sigX aNode bNode) = MkNode2 (sF, sX) (sfr $ \r (a, b) (sF', sX') -> let
  --   (f, new_sF) = runSF sigF r () sF'
  --   (x, new_sX) = runSF sigX r (a, b) sX'
  --   in (f x, (new_sF, new_sX))) aNode bNode
  -- (MkNode2 sF sigF aNode bNode) <*> (MkNode0 sX sigX) = MkNode2 (sF, sX) (sfr $ \r (a, b) (sF', sX') -> let
  --   (f, new_sF) = runSF sigF r (a, b) sF'
  --   (x, new_sX) = runSF sigX r () sX'
  --   in (f x, (new_sF, new_sX))) aNode bNode
  -- (MkNode2 sF sigF aNodeF bNodeF) <*> (MkNode2 sX sigX aNodeX bNodeX) = MkNode2 (sF, sX) (sfr $ \r ((aF, bF), (aX, bX)) (sF', sX') -> let
  --   (f, new_sF) = runSF sigF r (aF, bF) sF'
  --   (x, new_sX) = runSF sigX r (aX, bX) sX'
  --   in (f x, (new_sF, new_sX))) ((,) <$> aNodeF <*> bNodeF) ((,) <$> aNodeX <*> bNodeX)

instance Show (Node x) where
  show node = let
    editShowState str = if str == "()" then "" else str
    editShowNode str = if str == "()" then "_" else "(" <> str <> ")"
    showState s = "{" <> editShowState (show s) <> "}"
    showBinNode aNode bNode = editShowNode (show aNode) <> " " <> editShowNode (show bNode)
    in case node of
      (MkNode0 s _) -> let str = "N0 " <> showState s in if str == "N0 {}" then "" else str
      (MkNode2 s _ aNode bNode) -> "N2 " <> showState s <> " " <> showBinNode aNode bNode
      (MkNodeC s _ aNode fNode) -> "NC " <> showState s <> " " <> showBinNode aNode fNode

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
      (!a, !new_aNode) = go aNode
      (!b, !new_bNode) = go bNode
      (!x, !new_s) = a `seq` b `seq` runSF sig r (a, b) s
      in x `seq` new_aNode `seq` new_bNode `seq` (x, MkNode2 new_s sig new_aNode new_bNode)
    go (MkNodeC !s !sig !aNode !fNode) = let
      (!a, !new_aNode) = go aNode
      (!ebx, !new_s) = a `seq` runSF sig r a s
      (!x, !new_fNode) = case ebx of
        Right xDefault -> (xDefault, fNode)
        Left !b -> let (!f, !new_fNode') = go fNode in f `seq` b `seq` new_fNode' `seq` (f b, new_fNode')
      in x `seq` new_s `seq` new_aNode `seq` new_fNode `seq` (x, MkNodeC new_s sig new_aNode new_fNode)

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
          , getDeltaTime = (1 / fromIntegral sampleRate)
          , getChanMode = chanMode
          }

-- | Constructor for a unary node, derived from `MkNode2`.
mkNode1 :: Show s => s -> SF a s x -> Node a -> Node x
mkNode1 s sig node = MkNode2 s (sfr $ \r (a, _) -> runSF sig r a) node uNode
{-# INLINABLE mkNode1 #-}

-- | Constructor for a ternary node, derived from `MkNode2`. (This pattern of tupling up arg
-- nodes can be extended indefinitely to higher arities if we ever need them.)
mkNode3 :: Show s => s -> SF (a, b, c) s x -> Node a -> Node b -> Node c -> Node x
mkNode3 s sig aNode bNode = MkNode2 s (sfr $ \r ((a, b), c) -> runSF sig r (a, b, c))
  $ liftA2 (,) aNode bNode

-- | Trivial node.
uNode :: Node ()
uNode = pure ()
{-# INLINABLE uNode #-}

-- | Generic accumulator node, with a `start` state plus an accumulator `accumFn`. The state
-- updates every time a value is emitted; the emitted value is that state.
accum :: Show s => s -> (i -> s -> s) -> Node i -> Node s
accum start accumFn = mkNode1 start $ sfArgs >>= \i -> modify (accumFn i) *> get

-- | Simple counter/ticker, from a `start` value and an `increment` from a node.  That state
-- is then emitted.
tick :: (Num a, Show a) => a -> Node a -> Node a
tick start = mkNode1 start $ sfArgs >>= \i -> state $ \s -> (s, s + i)

-- -- test stuff
-- __testenv = (0.0 :: TimeVal, 1.0 :: TimeVal)

-- __stepper :: SigEnv -> SigEnv
-- __stepper = (\(t, d) -> (t + d, d))

-- __testApplic = iterateNode __testenv id 3 $ (\a b c -> a + b + c) <$> (tick 1 1) <*> (tick 2 1) <*> (tick 3 1)
-- __testPiecewise = iterateNode __testenv __stepper 14 $
--   startAt (-1) 2.0 (Just 5) $ piecewise 1.0 0 [(tick 10 1, 3), (tick 20 1, 2), (tick 30 1, 4)]

-- | Retrieve the time tick context from the signal environment, as a node.
timeNode :: Node TimeVal
timeNode = MkNode0 () sfTime
{-# INLINABLE timeNode #-}

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
infixr 5 +|
{-# INLINABLE (+|) #-}

-- | Scale a node by a scalar multiplier.
(*|) :: (Num a) => a -> Node a -> Node a
{-# SPECIALIZE (*|) :: SynthVal -> Node SynthVal -> Node SynthVal #-}
(*|) multiplier = fmap (multiplier *)
infixr 6 *|
{-# INLINABLE (*|) #-}

-- | Warp the time axis for a node using a function.
nodeModifyTime :: (TimeVal -> TimeVal) -> Node a -> Node a
nodeModifyTime f node = case node of
  (MkNode0 s sig) -> MkNode0 s (sfModifyTime f sig)
  -- must recursively modify time for the child nodes too since our signal function concerns
  -- only ourselves; the child nodes are evaluated before we are.
  (MkNode2 s sig aNode bNode) -> MkNode2 s (sfModifyTime f sig) (nodeModifyTime f aNode) (nodeModifyTime f bNode)
  (MkNodeC s sig aNode fNode) -> MkNodeC s (sfModifyTime f sig) (nodeModifyTime f aNode) (nodeModifyTime f fNode)
  where
    sfModifyTime :: (TimeVal -> TimeVal) -> SF a s x -> SF a s x
    sfModifyTime g = local (\((t, d), ar) -> ((g t, d), ar))

-- | Have a node start emitting at a start time (and possibly end after some duration), else
-- emitting a default empty value outside the allowed timespan.
startAt :: a -> TimeVal -> Maybe TimeVal -> Node a -> Node a
startAt empt start maybeDur = MkNodeC () (sfTime <&> \t ->
  if t < start then Right empt else case maybeDur of
    Nothing -> Left (); Just dur -> if t < (start + dur) then Left () else Right empt) uNode
  . fmap const . nodeModifyTime (subtract start)

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
-- is centered, and 1.0 is hard right. (Though, no bounds checking is done to ensure this.)
stereoize' :: (Num a) => a -> Either (Node a) (Node (LR a)) -> Node (LR a)
stereoize' pan (Left monoNode) = fmap (\a -> MkLR (((1 - pan) * a), (pan * a))) monoNode
stereoize' pan (Right stereoNode) = fmap (\(MkLR (al, ar)) -> let a = al + ar in MkLR (((1 - pan) * a), (pan * a))) stereoNode

-- | mono2stereo. Convenience to turn a mono real-valued signal into a stereo signal.
m2s :: (Fractional a) => Node a -> Node (LR a)
m2s = stereoize' 0.5 . Left

-- | The node-parameterized version of `stereoize'`, where pan values may be from a node and
-- not constant.
stereoize :: (Num a) => Node a -> Either (Node a) (Node (LR a)) -> Node (LR a)
stereoize panNode (Left monoNode) = (\pan a -> MkLR (((1 - pan) * a), (pan * a))) <$> panNode <*> monoNode
stereoize panNode (Right stereoNode) = (\pan (MkLR (al, ar)) -> let a = al + ar in MkLR (((1 - pan) * a), (pan * a))) <$> panNode <*> stereoNode

twopi :: SynthVal
twopi = 2 * pi

-- | Shared signal function for sin oscillators.
_sinOscFn :: (SynthVal -> SynthVal) -> TimeVal -> Freq -> Vol -> SynthVal -> SynthVal -> (Maybe SynthVal) -> (SynthVal, (SynthVal, Maybe SynthVal))
_sinOscFn sinFn d f vol phase myAngle cachedDx2pi = let
  deltaTimes2pi = maybe (d * twopi) id cachedDx2pi
  angleDelta = f * deltaTimes2pi
  newAngle = myAngle + angleDelta
  newAngleNormed = if newAngle > twopi then newAngle - twopi else newAngle
  in (vol * sinFn (myAngle + phase), (newAngleNormed, Just deltaTimes2pi))
{-# INLINABLE _sinOscFn #-}

-- | Sine oscillator, holding state for the current angle.
-- Based on https://juce.com/tutorials/tutorial_sine_synth/.
sinOsc :: Node Freq -> Node Vol -> Node SynthVal
sinOsc = MkNode2 (0.0, Nothing) . sfr $ \(_, d) (f, vol) (myAngle, cachedDx2pi) ->
  _sinOscFn sin d f vol 0 myAngle cachedDx2pi

-- | `sinOsc` but accepts phase as an input node (added to the angle of each sin compute)
sinOscP :: Node Freq -> Node Vol -> Node SynthVal -> Node SynthVal
sinOscP = mkNode3 (0.0, Nothing) . sfr $ \(_, d) (f, vol, phase) (myAngle, cachedDx2pi) ->
  _sinOscFn sin d f vol phase myAngle cachedDx2pi

-- | Sine oscillator that uses a precomputed sine wavetable
-- based on https://juce.com/tutorials/tutorial_wavetable_synth/
-- wavetableSinOsc :: Int -> Node Freq -> Node Vol -> Node SynthVal
-- wavetableSinOsc tableSz = MkNode2 (makeWavetable sin)

-- | Square waveform signal; supports modulation of frequency, volume, and time
-- scale. (Duty cycle might become a parameter later down the line)
-- squareSource :: SF s Freq -> SF s Vol -> SF s SynthVal
-- squareSource = liftM3 (\t f v -> let ft = f * t in v * fromIntegral
--   ((2 * (2 * floor ft - floor (2 * ft)) + 1) :: Int)) getTime

-- | A @fold@/@mapAccum@ helper for `piecewise`, but may be useful elsewhere. Will carry out
-- a foldl with a base accumulator value and an accumulator func but also takes a "decorator
-- function" as argument to combine the current list item with the current accumulator value
-- and obtain an item in the output list (the type @b@ in the function's type.) A common use
-- case is to "decorate" the list with the accumulator value alongside folding. Outputs both
-- the accumulator result and the resulting decorated list.
-- Example usage:
-- >>> cascadeMap 0 (\acc (c, n) -> acc + n) (\acc (c, n) -> (c, n, acc))
-- >>>    $ [('a',1),('b',5),('c',3)]
-- ([('a',1,0),('b',5,1),('c',3,6)],9)
cascadeMap :: x -> (x -> a -> x) -> (x -> a -> b) -> [a] -> ([b], x)
cascadeMap baseAccumVal _ _ [] = ([], baseAccumVal)
cascadeMap baseAccumVal accumFn decorateFn (a:as) =
  let newAccumVal = accumFn baseAccumVal a
      (rList, rAccum) = newAccumVal `seq` cascadeMap newAccumVal accumFn decorateFn as
  in  ((decorateFn baseAccumVal a):rList, rAccum)

-- | Sequence nodes together given the durations they should each play for.
piecewise :: TimeVal
            -- ^the start time for this sequence of nodes
          -> a -- ^empty value to emit when no nodes are scheduled to be playing
          -> [(Node a, TimeVal)]
            -- ^a list of @[(node, duration)]@; @node@ plays for @duration@, sequenced
            -- together in the order they're in this list
          -> Node a
piecewise t0 empt nodesDurs = MkNode0 lss (sfEnv >>= \(t, d) -> state $ customMap t (t, d))
  where
    -- Compiler is angry at any explicit type annotation for lss and customMap for some reason lol
    -- but lss should be [(Node a, TimeVal, TimeVal)], and customMap :: TimeVal -> SigEnv -> [(Node a, TimeVal, TimeVal)]
    lss = (fmap (\(node, dur, sta) -> (startAt empt sta (Just dur) node, dur, sta))) .  durs2dursStarts t0 $ nodesDurs
    customMap _ _ [] = (empt, [])
    customMap t r ls@((node, dur, sta):rest)
      | t < sta = (empt, ls)
      | t >= sta && t < (sta + dur) = let (x, newNode) = runNode r node in (x, (newNode, dur, sta):rest)
      | otherwise = customMap t r rest

-- Given a list [(node, start, duration)], sequence them and allow them to keep playing over
-- each other (i.e. if the list says node0 starts at t=0s and plays for 5s, and node1 starts
-- at t=2s and plays for 6s, then both should play together in the interval 2 <= t < 5. This
-- requires a combiner function to merge values from simultaneously playing nodes) (@a@ only
-- needs to be a @Monoid@ rather than a @Num@ but it is real annoying to wrap and unwrap the
-- numeric Sum monoid newtype so we'll just expect @Num@, won't hurt)
cascade :: (Traversable f, Num a)
          => TimeVal -- ^the start time for this sequence of nodes
          -> a -- ^empty value to emit when no nodes are scheduled to be playing
          -> f (Node a, TimeVal, Maybe TimeVal)
            -- ^a list of @[(node, start, maybe duration)]@; @node@ starts at @start@ and plays for @duration@ if specified, sequenced
          -> Node a
cascade t0 empt = sum . fmap (\(node, start, mdur) -> startAt empt (t0 + start) mdur node)

-- | given a starting time, and a list @[(a, duration)]@ of @a@ values to be strung together
-- back-to-back according to their duration, compute @[(a, starttime, duration)]@
durs2dursStarts :: Num t => t -> [(a, t)] -> [(a, t, t)]
durs2dursStarts t0 = fst . cascadeMap t0
  (\acc (_, dur) -> acc + dur)
  (\acc (node, dur) -> (node, dur, acc))

-- | This is what is understood as "playing a melody with a synth".  Effectively "resets and
-- retriggers" a node in sync with note data. This should apply to any node parameterized by
-- frequency and volume (i.e. a synth/instrument node); in addition, panning will be applied
-- since a note `Cell`s may specify pan.
retriggerWithNotes  :: TimeVal -- ^a start time for this retriggering sequence
                    -> LR SynthVal -- ^empty value for when notes have concluded
                    -> (Node Freq -> Node Vol -> Node (LR SynthVal))
                        -- ^instrument to retrigger in sync with notes,
                        -- parameterized by frequency and volume
                    -> [Cell] -- ^note data cells making up the melody
                    -> Node (LR SynthVal)
retriggerWithNotes start empt instrument cells = piecewise start empt
  ((\cell -> (
    (fmap dupLR (adsrEnvelope (adsrOf cell)) |*|) $
    doPanning (panOf cell) $
    instrument (pure $ freqOf cell) (pure $ volOf cell), durOf cell
  )) <$> cells)
  where
    doPanning maybePan = stereoize' (unmaybePan maybePan) . Right

-- | A ramp signal that increases from start to end (or possibly decreases) in a given range
-- of time based on some given function, then holds. For linear ramping, the function should
-- be \t -> t; For a quadratic ramp, the function is \t -> t*t etc.
funcRamp :: (Num v, Ord v) => (TimeVal -> v) -> TimeVal -> v -> v -> Node v
funcRamp f dur start end = if dur == 0 then pure end else timeNode <&> (\t ->
  let downramp = start > end; smaller = min start end; bigger = max start end;
      up = max smaller . min bigger $ smaller + (bigger - smaller) * (f $ t / dur)
  in if downramp then smaller + bigger - up else up)

-- | Specialization of funcRamp to a linear function
linearRamp :: TimeVal -> SynthVal -> SynthVal -> Node SynthVal
linearRamp = funcRamp id

-- | Create a signal that traverses an ADSR envelope according to some given envelope params
-- TODO redesign ADSR so that we can receive sustain time from a playing note with `cascade`
-- to allow release tails to overlap the next notes? rather than baking total time into ADSR
-- params themselves (which happens to also be what euterpea does)
adsrEnvelope :: ADSRParams -> Node SynthVal
adsrEnvelope adsrVals =
  piecewise 0.0 0.0
  [ (attackSig, at), (decaySig, dt), (pure susLvl, st), (releaseSig, rt) ]
  where
    at = attackTimeOf adsrVals
    dt = decayTimeOf adsrVals
    rt = releaseTimeOf adsrVals
    totalt = totalTimeOf adsrVals
    st = totalt - at - dt - rt
    -- note that ^ totalt doesn't have to match the note's playback time.
    -- you could set the Cell (note)'s duration to be really long while
    -- the envelope-totalt very short to traverse the envelope very quickly,
    -- followed by some quiet before the next note; i.e. staccato.
    susLvl = sustainLvlOf adsrVals
    attackSig = linearRamp at (startLvlOf adsrVals) 1.0 -- ramp up
    decaySig = linearRamp dt 1.0 susLvl -- ramp down
    releaseSig = linearRamp rt susLvl (endLvlOf adsrVals) -- ramp down
