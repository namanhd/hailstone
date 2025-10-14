# hailstone

***ha**skell **i**ntegrated **l**anguages for **s**ynthesizing **tone**s*

Audio synthesis + song composition embedded languages.


## Synthesis
(as of 2025-09)

### Signal functions and Nodes
The modular audio system consists of `Node`s, which are a tree-like structure, though a directed acyclic graph of audio nodes (nodes forking output to multiple nodes) can be represented with `cache` (more on this later.)

Each `Node x` packages a state `s` together with a "signal function" that reads the signal environment (i.e. current time, sample rate), its current state `s`, optionally an "argument" `a` emitted from some input/argument `Node a`, and returns an emitted value `x` and an updated state.

A signal function thus has the type
```haskell
type SFr r a s x = r -> a -> s -> (x, s)
```

> Of course this is essentially simply the `Reader` monad together with the `State` monad.
> ```haskell
> type SFr r a s x = ReaderT (r, a) (State s) x
> ```
> We don't actually write it as this monad stack in the code. This has a small cost and in this setting we need as little allocation and GC pressure as possible. Plus all the signal functions are written as pure functions that don't really need the ergonomic niceties offered by the `Reader` and `State` methods anyway.

We can specialize the signal environment type to `SigEnv = (TimeVal, TimeVal)` containing
`(currentTime, deltaTime = 1/samplerate)`, giving
```haskell
type SF a s x = SFr SigEnv a s x
```
A unit of audio processing isn't just a function, it needs to hold its own *persistent* state.
Just like how things go in OOP-land, an audio
unit/node "bundles together data and functions", and indeed many synthesis algorithms depend on
holding a running state as the audio node gets queried for new samples.

We'll need a way to make these nodes compose together as if they were pure values and
functions *yet* keep their running state as they use each other's values for
complicated signal processing graphs.

We define what amounts to an abstract syntax tree-like structure that decides the "term
grammar" of a "language". Each constructor in such a tree type represents a primitive/axiomatic
construct that an interpreter function for this "language" must explicitly handle. As such, we can come up with
some primitive ways to construct a `Node` that have both pure-like composability on the surface and statefulness underneath, keeping in mind their evaluation semantics to be implemented in a separate interpreter function.

```haskell
data Node x where
  MkNode0 :: s -> SF () s x -> Node x
  -- ^nullary node, representing a source of values with no arguments needed.
  MkNode2 :: s -> SF (f, a) s x -> Node f -> Node a -> Node x
  -- ^binary node. can represent any time-varying computation taking two argument nodes, but
  -- can also be used to lift, into a node, via the applicative instance, the application of
  -- a pure function @f@ to a pure argument @a@.
```
> Note that the state type variable `s` does not show up on the left hand side of the data declaration. This makes `s` existentially quantified, i.e. `forall s. s -> ...`. Users of the `Node x` type cannot pattern match or know anything about the particular `s` type in use, only that it is compatible with the signal function `SF _ s x` it is bundled with.
>
> This makes sense, as implementations of audio nodes need not expose to the outside world what their internal state is or how it is used. Different nodes in a node graph are sure to have different state needs internally, so it is only natural to hide `s` away rather than having it be parameterized in the datatype i.e. `Node s x`.
>
> This is essentially encapsulation, where we've hidden the gory implementation details of statefulness, its type, and indeed even its presence.

And here's the interpreter, which produces one sample from the node tree and returns the tree but with updated state values. A node queries values from its children, using them for its own computation.
```haskell
runNode :: SigEnv -> Node x -> (x, Node x)
runNode r = go
  where
    go :: Node x -> (x, Node x)
    go (MkNode0 s sig) = let
      (x, new_s) = sig r () s in (x, MkNode0 new_s sig)
    go (MkNode2 s sig aNode bNode) = let
      (a, new_aNode) = go aNode
      (b, new_bNode) = go bNode
      (x, new_s) = sig r (a, b) s
      in (x, MkNode2 new_s sig new_aNode new_bNode)
```

The implementation in `Sound/Hailstone/Synth.hs` has more specialized constructors and `SF` types for efficiency (to not require dummy state, etc), but these ideas still hold. This essentially lets us fairly directly translate object-oriented DSP code into our `Node` representation while preserving composability on the surface with the provided nodes and combinators.

> There's also a nice Applicative instance we get almost for free, useful for lifting pure code into nodes.
> 
> We could probably swizzle this type into an Arrow; packaging static state together with a state-transforming function was one of the [motivating examples for Hughes's Arrows](https://www.cse.chalmers.se/~rjmh/afp-arrows.pdf). Arrows are also the primary abstraction used by e.g. [Euterpea](https://hackage.haskell.org/package/Euterpea), supporting a "native" syntax for defining signal graphs. However, arrows are somewhat clunky, have some overhead in the combinators, and so far I am doing fine with just the applicative instance. I will explore this later if it turns out to become necessary.

### Stateful oscillators

As an example of how close this is to the usual OOP representation of audio units, here's a sine oscillator that holds its current angle as state (a *phase accumulator*, important for correct pitch sliding and other synthesis), translated nearly directly from https://juce.com/tutorials/tutorial_sine_synth/.

```haskell
sinOsc :: Node Ampl -> Node Freq -> Node SynthVal
sinOsc = MkNode2 0.0 $ \(_, d) (a, f) angleAccum -> let
  angleDelta = f * (d * twopi)
  newAccum' = angleAccum + angleDelta
  newAccum = if newAccum' > twopi then newAccum' - twopi else newAccum'
  in (a * sin angleAccum, newAccum)
```

This is a function that takes two nodes (an amplitude-emitting node and a frequency-emitting node) and returns a stateful node that computes a sine wave, with state behind the scenes (the phase accumulator, initialized with 0.0) that the caller of this function does not have to explicitly handle.

As the parameters to this node are themselves nodes, only function application is needed to route them to this node, with correctly-handled states when the tree is finally evaluated.

For instance, we can specify a `sinOsc` to be used to modulate the frequency of another `sinOsc`, for vibrato or perhaps FM synthesis. This defines a sin oscillator that has a 10Hz sinusoidal vibrato with amplitude 0.02 applied to the frequency by adding 1 and multiplying.
```haskell
sinWithVibrato :: Node Ampl -> Node Freq -> Node SynthVal
sinWithVibrato a f = sinOsc a (f * (1 +| sinOsc 0.02 10))
```

We can also make the vibrato frequency ramp up linearly over time by making the frequency argument of the modulating `sinOsc` be a `linearRamp`, going from 5Hz to 12Hz in 1.2 seconds:

```haskell
sinWithRampingVibrato :: Node Ampl -> Node Freq -> Node SynthVal
sinWithRampingVibrato a f = sinOsc a (f * (1 +| sinOsc 0.02 (linearRamp 1.2 5 12)))
```

> Nodes have a `Num` and `Fractional` instance, so we can just specify bare number literals and they will get automatically lifted to constant `Node`s emitting those values. Likewise for default math binary operations which are supported, though specialized for scalar-node adds/multiplies there are also the `(+|)` and `(*|)` operators.

### Sequencing  & playing notes
The `Node` representation, in addition to signal processing and synthesis, can also do larger-scale sequencing.
Nodes can be sequenced
- `piecewiseMono` (back-to-back, no overlap/monophonic)
- `piecewisePoly` (allocating nodes into voices of non-overlapping segments, processing each voice with `piecewiseMono`, then summing the results)

```haskell
piecewiseMono :: TimeVal -> a -> [(Node a, TimeVal, Timeval)] -> Node a
piecewisePoly :: TimeVal -> a -> [(Node a, TimeVal, Timeval)] -> Node a
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

An *instrument* is any function parameterized by a Node emitting `LiveCell` values. A `LiveCell` describes the instantaneous playing parameters of a note, such as pitch, volume/amplitude, envelope progress, and potentially other values; a stream of `LiveCell` is rendered from the higher-level static `Cell` specification of a note.

This way, instruments/synths can react in their own way to a note's volume, envelope, or any other high-level modulating parameter. We'll also have room to implement e.g. note-level effects with per-note parameters such as per-note
vibrato or portamento.

### Node graphs

The Node datatype is really just a binary tree. However, audio nodes/units like the ones in the Web Audio API, or any other modular audio environment such as Pd or Max, are nodes in an *audio graph*, not just a tree. A Node should be able to direct its output to multiple nodes (i.e. have multiple parents), which is impossible to represent with a tree.

#### Observable sharing at the EDSL level
It turns out that we can add this feature to our tree datatype using an old EDSL-building trick, *observable sharing*, which makes visible to the embedded language the host (embedding) language's name bindings and value reuse. There's a good [writeup on this](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/doc/design/design.md) and [its implementation in reactive-banana](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Prim/High/Cached.hs), with relevant citations. All proposed realizations in Haskell involve using `unsafePerformIO` to keep a sort of private IO mutable variable in each initialization of an object to be shared. 

*This is a slimy hack that breaks referential transparency* (on purpose), but it is useful, self-contained, and grants compute efficiency. In our case, as long as nodes are run via `runNode` with a properly-incremented `SigEnv`, this won't change the denotation of the program; i.e. using `cache` should not change the resulting audio; it merely improves performance.

I've implemented this as the function `cache :: Node x -> Node x`. In a portion of code such as
```haskell
let ff        = cache f  -- f :: Node Freq
    modulator = cache (sinOsc 0.5 (3 *| ff))
    carrier1  = sinOscP 1.0 ff modulator
    carrier2  = sinOscP 0.25 (2 *| ff) modulator
    finalNode = carrier1 + carrier2
in finalNode
```
The frequency-emitting node `f` is converted into a caching/memoized node with `ff = cache f`, and using the name `ff` now enables the same value computed once from `f` to be reused in later nodes that take that node as an argument.

(Without `cache`, the node `f` would be duplicated across all its use sites, as that's the only representation of this relationship in a binary tree rather than a DAG. This would result in repeated computation of `f`, and if that computation is taxing (i.e. if `f` is itself is the head of a huge node tree) then this will quickly grow to become infeasible to compute at speed.)

Likewise, `modulator` is made a caching node, and its output is the shared phase modulation input for two other sine oscillators. Caching allows genuine reuse of the modulator value, which is now computed only once each sampling step.

#### Morally right way to do signal forks
Of course, `cache` is meant for the higher-level EDSL user-facing interface of composing nodes together.
If one is up for the task of writing new nodes/node functions (such as the library implementer, or an enterprising EDSL user), the better way is to directly write a new node function with this logic in the signal function itself. For instance, one could define a bespoke node like the sine oscillator node example given above. Its signal function might use the same value from an argument node for multiple math operations; inside the signal function is normal pure Haskell code operating on real values, so normal, expected sharing semantics apply.


## Composition
TODO!


## Audio backend

### SDL audio
Generating samples only within the SDL audio callback is way too brittle and prone to GC stops. This causes underruns and crackling even at very high buffer sizes.

Instead, we run a separate "producer" thread that writes samples to a concurrent channel/queue/ringbuffer (using [unagi-chan](https://hackage.haskell.org/package/unagi-chan)'s unboxed channel implementation for fewer allocations, with a queue size bound we impose manually). Then, the SDL audio callback's only job is to be the "consumer" of that queue, copying samples over to the SDL-side buffer to be played.

This is a cushion that lets our sample-generating node graph be free to run ahead of the SDL-fired audio callback, so that any lag or GC hiccup during sample generation has minimal effect on audio consistency. *As long as the producer thread maintains a consistent lead over the audio callback*, there won't be any underruns due to instantaneous producer-side hiccups.

In practice, there may still be tiny hiccups, especially during the first instantaneous moments of playback.
A tiny delay before enabling the callbacks may help; some RTS options-tweaking also appears effective, particularly parallel GC
with the `-N` option (which makes available all CPU threads up to `-maxN` threads; though just 2 seems good and memory-efficient.)

These settings are in the `hailstone.cabal` file as the default rtsopts to compile with:
```
-N -maxN2 -H20m
```
(use a max of 2 cores/hyperthreads, and allow a 20M "suggested heap size".)

(the size of `-A` and `-H` can also be adjusted depending on the program; some experimentation needed. Keep `-s` to view the stats, omit it otherwise.)

### Offline/other backends

There will be simple wav-exporting backends implemented at some point, probably when I start experimenting with audio graphs much larger than what can be handled live/near-real-time.

## Issues and drawbacks

It is really hard to get predictable, fast performance for live audio applications in pure Haskell. Most everyone has given up on getting end-to-end Haskell audio to be real-time capable; even the [venerable co-author of Euterpea and the  Haskell School of Music](https://www.donyaquick.com/vivid-as-a-real-time-audio-solution-for-euterpea/) has sworn off attempting real-time in pure Haskell.

That said, I still think this is really interesting to work through for learning both Haskell and DSP.
With some effort in adding strictness annotations, reducing boxing and representing state using simpler types, we can reduce space leaks and relieve GC lag.

The `Node` representation is a big step up from my previous attempts (4 years ago... ouch.) Before this I'd hopped around representations, first lazy infinite lists, which had free memoization by the runtime but were extremely allocation-heavy, then the simple `t -> x` reader monad which did not have statefulness that could compose. It's cool to have landed on something that seems to address these issues, though there will be definitely many more problems.

For any nontrivial audio processing, this will probably still need to be offline rather than real-time. That said, I'm doing my testing via SDL audio to play audio as the program runs.

## Organization
The library is in `src/`, and has no dependencies other than `base` at the moment. You can load the `Sound.Hailstone.Synth` module in GHCi or another Main to try it out directly, no `stack` or package installations needed.

The executable is in `exe/SDLTestMain.hs`, which is
where we write test songs and synths to play around with. It depends on `sdl2`, `unagi-chan`, and `vector`, and should probably be built using the `stack` instructions below.

## Building
```
stack build
stack run
```
Requires SDL2 to be installed on the system. (also, the included `stack.yaml`
uses the system ghc by default, which you might want to change depending on the
config)

Current `stack` resolver is `lts-24.10` (ghc 9.10.2).
However, this should work with newer ghcs and resolvers just fine.