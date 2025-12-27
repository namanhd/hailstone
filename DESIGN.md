# hailstone design overview

## Synthesis

### Signal functions and Nodes
The modular audio system consists of `Node`s, which are a tree-like structure, though a directed acyclic graph of audio nodes (nodes forking output to multiple nodes) can be represented with `share` (more on this later.)

An audio node packages a state `s` together with a "signal function" that
reads the signal environment (i.e. current time, sample rate, and other
outside-world information `e`), its current state `s`, optionally an "argument"
`a` emitted from some input/argument `Node a`, and returns an emitted value `x`
and an updated state. In other words, an audio node is a Mealy machine.

A signal function thus has the approximate type
```haskell
SigEnv e -> a -> s -> (x, s)
```
where `data SigEnv e = MkSigEnv { t :: TimeVal, d :: TimeVal, e :: e }`.

> This can be written as the monad stack
> ```haskell
> type SFr e a s x = ReaderT (SigEnv e, a) (State s) x
> ```
> We don't actually write it as this monad stack in the code; it has a small
> cost (mostly due to typeclass dictionary passing?), and we don't explicitly use the reader and state methods offered by the
> mtl types so there's no need.

We then need to define a data structure to hold these functions, their relations to each other (to compose them), and the
persistent state they're bundled with. We define what amounts to an abstract
syntax tree-like structure that decides the "term grammar" of a "language" for which we then write an
interpreter function.

```haskell
data Node e x where
  MkNode0 :: s -> (SigEnv e -> s -> (x, s)) -> Node e x
  -- ^nullary node, representing a source of values with no arguments needed.
  MkNode2 :: s -> (SigEnv e -> a -> b -> s -> (x, s)) -> Node e a -> Node e b -> Node e x
  -- ^binary node. can represent any time-varying computation taking two argument nodes, but
  -- can also be used to lift, into a node, via the applicative instance, the application of
  -- a pure function @f@ to a pure argument @a@.
```
> The state type variable `s` is existentially quantified. Users of the `Node x` type cannot pattern match or know anything about the particular `s` type in use, only that it is compatible with the signal function it is bundled with.
>
> This makes sense, as implementations of audio nodes need not expose to the outside world what their internal state is or how it is used. Different nodes in a node graph are sure to have different state needs internally, so it is only natural to hide `s` away rather than having it be parameterized in the datatype i.e. `Node e s x`.
>
> This is encapsulation, where we've hidden the gory implementation details of statefulness, its type, and indeed even its presence.

And here's the interpreter, which produces one sample from the node tree and returns the tree but with updated state values. A node queries values from its children, using them for its own computation.
```haskell
runNode :: SigEnv -> Node e x -> (x, Node e x)
runNode r node = case node of
  (MkNode0 s sig) -> let
    (x, new_s) = sig r s in (x, MkNode0 new_s sig)
  (MkNode2 s sig aNode bNode) -> let
    (a, new_aNode) = runNode r aNode
    (b, new_bNode) = runNode r bNode
    (x, new_s) = sig r a b s
    in (x, MkNode2 new_s sig new_aNode new_bNode)
```

The implementation in `Sound/Hailstone/Synth/Node.hs` has more specialized constructors and function types for efficiency (to not require dummy state, to support mutable IO, etc), but these ideas still hold. This essentially lets us fairly directly translate object-oriented DSP code into our `Node` representation while preserving composability on the surface with the provided nodes and combinators.

> There's also a nice Applicative instance we get almost for free, useful for lifting pure code into nodes.
>
> We could probably swizzle this type into an Arrow; packaging static state together with a state-transforming function was one of the [motivating examples for Hughes's Arrows](https://www.cse.chalmers.se/~rjmh/afp-arrows.pdf). Arrows are also the primary abstraction used by e.g. [Euterpea](https://hackage.haskell.org/package/Euterpea), supporting a "native" syntax for defining signal graphs. However, arrows are somewhat clunky, have some overhead in the combinators, and so far I am doing fine with just the applicative instance. I will explore this later if it turns out to become necessary.

### Node definition context
Sometimes writing the definition or the initial state for a node requires information such as the delta time (1/sample rate) or IO to initialize a mutable delay buffer.
This must be provided at node definition time, not at evaluation time. As such, we define some traits, namely `MonadHasDeltaTime`, `MonadHasDelayQueue`, `MonadHasSharing`, that flag what special abilities are needed to define and initialize a `Node`.

> The signal environment also contains delta time (and IO depending on the Node constructor), but this can only accessed from within the signal function of a node, not in the code that defines and initializes it.

At the end, all of these abilities are present in the `NodeDefnCtx` concrete monad (a `ReaderT` with `IO`), but node definition writers need not care what the implementation is.

### Stateful oscillators

As an example of how close this is to the usual OOP representation of audio units, here's a sine oscillator that holds its current angle as state (a *phase accumulator*, important for correct pitch sliding and other synthesis), translated nearly directly from https://juce.com/tutorials/tutorial_sine_synth/.

```haskell
sinOsc :: Node e Ampl -> Node e Freq -> Node e SynthVal
sinOsc = MkNode2 0.0 $ \(MkSigEnv { d = deltaTime }) amplitude frequency angleAccum -> let
  angleDelta = frequency * (deltaTime * twopi)
  newAccum' = angleAccum + angleDelta
  newAccum = if newAccum' > twopi then newAccum' - twopi else newAccum'
  in (amplitude * sin angleAccum, newAccum)
```

This is a function that takes two nodes (an amplitude-emitting node and a frequency-emitting node) and returns a stateful node that computes a sine wave, with state behind the scenes (the phase accumulator, initialized with 0.0) that the caller of this function does not have to explicitly handle.

As the parameters to this node are themselves nodes, only function application is needed to route them to this node, with correctly-handled states when the tree is finally evaluated.

For instance, we can specify a `sinOsc` to be used to modulate the frequency of another `sinOsc`, for vibrato or perhaps FM synthesis. This defines a sin oscillator that has a 10Hz sinusoidal vibrato with amplitude 0.02 applied to the frequency by adding 1 and multiplying.
```haskell
sinWithVibrato :: Node e Ampl -> Node e Freq -> Node e SynthVal
sinWithVibrato a f = sinOsc a (f * (1 +| sinOsc 0.02 10))
```

We can also make the vibrato frequency ramp up linearly over time by making the frequency argument of the modulating `sinOsc` be a `linearRamp`, going from 5Hz to 12Hz in 1.2 seconds:

```haskell
sinWithRampingVibrato :: Node e Ampl -> Node e Freq -> Node e SynthVal
sinWithRampingVibrato a f = sinOsc a (f * (1 +| sinOsc 0.02 (linearRamp 1.2 5 12)))
```

> Nodes have a `Num` and `Fractional` instance, so we can just specify bare number literals and they will get automatically lifted to constant `Node`s emitting those values. Likewise for default math binary operations which are supported, though specialized for scalar-node adds/multiplies there are also the `(+|)` and `(*|)` operators.

### Sequencing  & playing notes
The `Node` representation, in addition to signal processing and synthesis, can also do larger-scale sequencing.
Nodes can be sequenced
- `piecewiseMono` (back-to-back, no overlap/monophonic)
- `piecewisePoly` (allocating nodes into voices of non-overlapping segments, processing each voice with `piecewiseMono`, then summing the results)

```haskell
piecewiseMono :: TimeVal -> a -> [(Node e a, TimeVal, Timeval)] -> Node e a
piecewisePoly :: TimeVal -> a -> [(Node e a, TimeVal, Timeval)] -> Node e a
```

So an expression like
```haskell
piecewisePoly 5.0 0.0 [(node1, 0.0, 10.0), (node2, 0.5, 2.0)]
```
will create a node that
- emits silence (the `0.0` argument) until time `t=5.0`
- starts the sequence of nodes from `node1` which plays for `10.0` seconds from `t=5.0`
- then `node2` starting at `t=5.5` which plays for `2.0` seconds, with
`node1` still playing while it does.

> Note that the nodes in the list specify their start time relative to the `t0` argument of `piecewisePoly`.


Built on top of `piecewisePoly`, the function `retriggerWith` plays a melody
using a "synth/instrument". This is done by "restarting" an instrument node on every new note, rather than letting the instrument sound evolve independently of the notes.

An *instrument* is any function parameterized by a Node emitting `Now` values. A `Now` describes the instantaneous playing parameters of a note, such as pitch, volume/amplitude, envelope progress, and potentially other values; a stream of `Now` is rendered from the higher-level static `Cell` specification of a note.

This way, instruments/synths can react in their own way to a note's volume, envelope, or any other high-level modulating parameter. We'll also have room to implement e.g. note-level effects with per-note parameters such as per-note
vibrato or portamento.

### Node graphs

The Node datatype is really just a binary tree. However, audio nodes or units in most APIs are in an *audio graph*, not just a tree. A Node should be able to direct its output to multiple nodes (i.e. have multiple parents), which is impossible to represent with a tree.

#### Observable sharing at the EDSL level
We can add this feature to our tree datatype using *observable sharing*, which makes visible to the embedded language the host (embedding) language's name bindings and value reuse. There's a good [writeup on this](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/doc/design/design.md) and [its implementation in reactive-banana](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Prim/High/Cached.hs), with relevant citations.

In my implementation, I've implemented this as the function `share :: MonadHasSharing m => Node e x -> m (Node e x)`, which requires the ability to initialize an IORef (though this is merely a code hygiene thing to isolate this from full-blown IO).
In a portion of code such as
```haskell
do
  ff         <- share f
  modulator  <- share (sinOsc 0.5 (3 *| ff))
  let
    carrier1  = sinOscP 1.0 ff modulator
    carrier2  = sinOscP 0.25 (2 *| ff) modulator
    finalNode = carrier1 + carrier2
  return finalNode
```
The frequency-emitting node `f` is converted into a caching/memoized node with `ff = share f`, and using the name `ff` now enables the same value computed once from `f` to be reused in later nodes that take that node as an argument.

(Without `share`, the node `f` would be duplicated across all its use sites, as that's the only representation of this relationship in a binary tree rather than a DAG. This would result in repeated computation of `f`, and if that computation is taxing (i.e. if `f` is itself is the head of a huge node tree) then this will quickly grow to become infeasible to compute at speed.)

Likewise, `modulator` is made a caching node, and its output is the shared phase modulation input for two other sine oscillators. Caching allows genuine reuse of the modulator value, which is now computed only once each sampling step.

Another example: an echo effect with filtering and panning applied to the wet signal.

```haskell
echo delayMs decayMult wetLvl filterQ filterF wetPan input = do
  input' <- share input
  filteredDelayOut <- lpf filterQ filterF =<< echoRaw delayMs decayMult input'
  let output = (1 - wetLvl) *|| input' + (balance wetPan $ wetLvl *|| filteredDelayOut)
  pure output
```

#### Morally right way to do signal forks
Of course, `share` is meant for the higher-level EDSL user-facing interface of composing nodes together.
If one is up for the task of writing new nodes/node functions (such as the library implementer, or an enterprising EDSL user), the better way is to directly write a new node function with this logic in the signal function itself.

For instance, one could define a bespoke "fused" node implementing the `echo` example. Its signal function might use the same value from an argument node for multiple math operations. Inside the signal function is normal pure Haskell code operating on real values, so normal, expected sharing semantics apply. Check out `echo'` in `Sound.Hailstone.Synth.Effect` to see this implementation.


## Composition
See `src/Sound/Hailstone/Sequencing/CellScript.hs` for a prototype DSL for stateful sequencing.
Writeup TODO.


## Audio backend

**The primary real-time audio backend is PortAudio**, but there's also a backend implementation
that uses SDL audio instead (though SDL has a much bigger dependency footprint; this can be enabled with a compilation flag; see `hailstone.cabal`.)

Both backends
fire a callback function periodically to fill a buffer of samples; our job is to fill this
buffer in time for when it's needed for playback.

*Generating samples only within the audio callback is way too brittle and prone to GC stops.* This causes underruns and crackling even at very high buffer sizes.

Instead, we run a separate "producer" thread that writes samples to a concurrent channel/queue/ringbuffer.
Then, a backend audio callback's only job is to be the "consumer" of that queue, copying samples over to a buffer to be played.

This is a cushion that lets our sample-generating node graph be free to run ahead of the backend-fired audio callback, so that any lag or GC hiccup during sample generation has minimal effect on audio consistency. *As long as the producer thread maintains a consistent lead over the audio callback*, there won't be any underruns due to instantaneous producer-side hiccups.

In practice, there may still be tiny hiccups, especially during the first instantaneous moments of playback.
Padding a tiny bit of silence before the actual audio may help; some RTS options-tweaking also appears effective, particularly parallel GC
with the `-N` option (which makes available all CPU threads up to `-maxN` threads; though just 2 seems good and memory-efficient.)

These settings are in the `hailstone.cabal` file as the default rtsopts to compile with:
```
-N -maxN2 -H20m
```
(use a max of 2 cores/hyperthreads, and allow a 20M "suggested heap size".)

(the size of `-A` and `-H` can also be adjusted depending on the program; some experimentation needed. Keep `-s` to view the stats, omit it otherwise.)

#### SIMD math
Effects processing is basically specialized for stereo pairs of `Double`s, which allows for some further hand-rolled optimizations.

Specifically, I've defined specialized unboxed SIMD representations of the stereo pair type. (This really has less to do with speeding up arithmetic and more to do with reducing boxing as much as possible, since the GC is the enemy of real-time audio here.) The code for this is in `Sound.Hailstone.Synth.LR`; this has only very recently been feasible with the native code generator starting from GHC 9.12.x.

### Offline/other backends

There will be simple wav-exporting backends implemented at some point, probably when I start experimenting with audio graphs much larger than what can be handled live/near-real-time.

### Issues

It is really hard to get predictable, fast performance for live audio applications in pure Haskell. Most everyone has given up on getting end-to-end Haskell audio to be real-time capable; even the [venerable co-author of Euterpea and the  Haskell School of Music](https://www.donyaquick.com/vivid-as-a-real-time-audio-solution-for-euterpea/) has sworn off attempting real-time in pure Haskell.

That said, with some effort in adding strictness annotations, specializing, reducing boxing and representing state using simpler types, we can reduce space leaks and relieve GC lag.
