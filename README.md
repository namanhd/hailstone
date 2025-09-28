# hailstone

haskell audio synthesis + song composition embedded languages

## Synthesis
At the moment, (as of 2025-09) the modular audio system consists of `Node`s, which are a tree-like structure.
Each `Node x` packages a state `s` together with a "signal function" `SF a s x` that reads the signal environment (i.e. current time, sample rate), its current state `s`, optionally an "argument" `a` emitted from some input/argument `Node a`, and returns an emitted value `x` and an updated state.

A signal function thus has the type
```haskell
type SFr r a s x = r -> a -> s -> (x, s)
```
but this is essentially simply the `Reader` monad together with the `State` monad. We can
lump the argument `a` in with the read environment type, so we can just say
```haskell
type SFr r a s x = ReaderT (r, a) (State s) x
```
We can specialize the signal environment type to `SigEnv = (TimeVal, TimeVal)` containing
`(currentTime, deltaTime = 1/samplerate)`, giving
```haskell
type SF a s x = SFr SigEnv a s x
```
This is all good, and captures what a pure "signal function" should do. However, an audio
system will need to have persistent state: just like how things go in OOP-land, an audio
unit/node "bundles together data and functions", and indeed many synthesis algorithms depend on
holding a running state as the audio node gets queried for new samples. As such, the type for
all our audio modules and functions will not simply involve the signal functions `SF`, but the `Node`, which bundles a state value together with a signal function.

We'll need a way to make these nodes compose together as if they were pure values and
functions *yet* keep their running state as they use each other's values for
complicated signal processing graphs.

There are probably a few different approaches, but the method I've chosen, as one does in any functional language,
is to define what amounts to an abstract syntax tree-like structure that decides the "term
grammar" of a "language". Each constructor in such a tree type represents a primitive/axiomatic
construct that an interpreter function for this "language" must explicitly handle. As such, we can come up with
some primitive ways to construct a `Node` that have both pure-like composability on the surface and statefulness underneath, keeping in mind their evaluation semantics to be implemented in a separate interpreter function.

```haskell
data Node x where
  MkNode0 :: s -> (SF () s x) -> Node x
  -- ^nullary node, representing a source of values with no arguments needed.
  MkNode2 :: s -> (SF (f, a) s x) -> (Node f) -> (Node a) -> Node x
  -- ^binary node. can represent any time-varying computation taking two argument nodes, but
  -- can also be used to lift, into a node, via the applicative instance, the application of
  -- a pure function @f@ to a pure argument @a@.
```
> Note that the state type variable `s` does not show up on the left hand side of the data declaration. This makes `s` existentially quantified, i.e. `forall s. s -> ...`. Users of the `Node x` type cannot pattern match or know anything about the particular `s` type in use, only that it is compatible with the signal function `SF _ s x` it is bundled with.
>
> This makes sense, as implementations of audio nodes need not expose to the outside world what their internal state is or how it is used. Different nodes in a node graph are sure to have different state needs internally, so it is only natural to hide `s` away rather than having it be parameterized in the datatype i.e. `Node s x`.
>
> This is essentially encapsulation, where we've hidden the gory implementation details of statefulness, its type, and indeed even its presence.

And here's the interpreter, which produces one sample from the node tree and returns the tree but with updated state values. (I've removed the strictness annotations sprinkled everywhere in the actual code):
```haskell
runNode :: SigEnv -> Node x -> (x, Node x)
runNode r = go
  where
    go :: Node x -> (x, Node x)
    go (MkNode0 s sig) = let
      (x, new_s) = runSF sig r () s in (x, MkNode0 new_s sig)
    go (MkNode2 s sig aNode bNode) = let
      (a, new_aNode) = go aNode
      (b, new_bNode) = go bNode
      (x, new_s) = runSF sig r (a, b) s
      in (x, MkNode2 new_s sig new_aNode new_bNode)
```

The comments in `Sound/Hailstone/Synth.hs` say more on the reasoning behind our choice of primitives/constructors and their evaluation semantics. However, this essentially lets us fairly directly translate object-oriented DSP code into our `Node` representation while preserving composability on the surface for users who use our provided nodes, combinators, and other functions.

> There's also a nice Applicative instance we get almost for free with `MkNode2`, useful for lifting pure code into nodes.
>
> However, a Monad instance is likely impossible for a similar reason [Swierstra & Duponcheel's LL(1) parser](https://dl.acm.org/doi/10.5555/647699.734159) doesn't have a Monad instance: the type packs static information (in our case, the node state; in their case, the "static parser") and in some sense we need to "preallocate state/static data" for every node/parser before the computation is executed, i.e. we need the ability to do static analysis. Yet the monadic bind for both the `Node` type and S&D's parser implies "dynamically producing static information on the fly during evaluation" for the *result of* `k` in `ma >>= k`, and there's no good way to do that. In both cases, however, the applicative interface is more than enough.
>
> There's an argument to be made to turn this into an Arrow; S&D's parser was indeed the [motivating argument for Hughes's Arrows](https://www.cse.chalmers.se/~rjmh/afp-arrows.pdf). It would be possible to try to convert the `Node` applicative to a proper Arrow, which is also the primary abstraction used by e.g. [Euterpea](https://hackage.haskell.org/package/Euterpea); however, arrows are somewhat clunky and so far I am doing fine with just the applicative instance. I will explore this later if it turns out to become necessary.
>
> One thing that arrows can do that applicatives on their own can't is **sharing** of an emitted value as the input to multiple other nodes, i.e. making the signal graph a directed acyclic graph (DAG) and not just a tree. As it is now, any time we bind a name to a node in the host language (Haskell) and use it in multiple places, that node gets *duplicated* (and thus its computation work repeated) at those use sites, rather than its output merely being reused as we might ideally want. The way to solve this without arrows is with **Observable Sharing**; Heinrich Apfelmus has a [good writeup regarding its use and implementation for reactive-banana](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/doc/design/design.md) (an FRP system that eschews arrows for a purely applicative interface.) This is something I'm interested in trying in at least some form--if not the black magic `unsafePerformIO` trick, then at least `Let` and `Var` constructs in the Node tree grammar.

For instance, here's a sine oscillator that holds its current angle as state, translated nearly directly from https://juce.com/tutorials/tutorial_sine_synth/.

```haskell
sinOsc :: Node Freq -> Node Gain -> Node SynthVal
sinOsc = MkNode2 0.0 . sfr $ \(_, d) (f, gain) myAngle -> let
  angleDelta = f * (d * twopi)
  newAngle = myAngle + angleDelta
  newAngleNormed = if newAngle > twopi then newAngle - twopi else newAngle
  in (gain * sin myAngle, newAngleNormed)
```

This is a function that takes two nodes (a frequency-emitting node and a gain-emitting node) and returns a stateful node that computes a sine wave, with state behind the scenes that the caller of this function  does not have to explicitly handle.

As the parameters to this node are themselves nodes, only function application is needed to build up a complex node tree representing complex modulations, with correctly-handled states when the tree is finally evaluated.

For instance, we can specify a `sinOsc` to be used to modulate the frequency of another `sinOsc`, for vibrato or perhaps FM synthesis. This defines a sin oscillator/instrument that has a 10Hz sinusoidal vibrato with amplitude 0.02 applied to the frequency by adding 1  and multiplying.
```haskell
sinWithVibrato :: Node Freq -> Node Vol -> Node SynthVal
sinWithVibrato f v = sinOsc (f * (1 + sinOsc 10 0.02)) v
```

We can also make the vibrato frequency ramp up linearly over time by making the frequency argument of the modulating `sinOsc` be a `linearRamp`, going from 5Hz to 12Hz in 1.2 seconds:

```haskell
sinWithRampingVibrato :: Node Freq -> Node Vol -> Node SynthVal
sinWithRampingVibrato f v = sinOsc (f * (1 + sinOsc (linearRamp 1.2 5 12) 0.02)) v
```

> Nodes have a `Num` and `Fractional` instance, so we can just specify bare number literals and they will get automatically lifted to constant `Node`s emitting those values. Likewise for default math binary operations which are supported, though specialized for scalar-node adds/multiplies there are also the `(+|)` and `(*|)` operators.


The `Node` representation, in addition to signal processing and synthesis, can also do larger-scale sequencing.
Nodes can be sequenced `piecewise` (i.e. back-to-back, no overlap/monophonic), or `cascade`, (i.e. simply splats nodes onto a timeline, summing them, allowing overlaps).

```haskell
piecewise :: TimeVal -> a -> [(Node a, TimeVal, Maybe Timeval)] -> Node a
cascade :: TimeVal -> a -> [(Node a, TimeVal, Maybe Timeval)] -> Node a
```

So an expression like
```haskell
cascade 5.0 0.0 [(node1, 0.0, Nothing), (node2, 0.5, Just 2.0)]
```
will create a node that emits silence (the `0.0` argument) until time `t=5.0`, at which point it starts the sequence of nodes starting with `node1` (note its start time `0.0` relative to when the sequence begins, i.e. `t=5`) which plays indefinitely (indicated by the `Nothing` duration), then `node2` starts at `t=5.5` (relative time `0.5`) which plays for `2.0` seconds.

- Built on top of `piecewise` or `cascade`, the function `retriggerWith` plays a melody
using a "synth/instrument" by restarting a synth node (any frequency and gain-parameterized
node) on every new note, effectively by creating a keystroke effect that retriggers and
modulates the synth signal on every new note rather than letting the synth sound evolve
independently of the notes.
- There's also `adsrEnvelope` (taking fixed ADSR parameters) and `adsrEnvelopeN`
(parameterized by a node emitting ADSR parameters) which create a signal that traverses an
Attack - Decay - Sustain - Release (ADSR) envelope, which can then be multiplied by
individual note-playing signals (for per-note ADSR), or entire summed signals (for
envelope-based modulation of gain, panning, even synthesis parameters across playback).

## Composition
TODO!

## Issues and drawbacks

Performance. Most everyone has given up on getting end-to-end Haskell audio to be real-time capable; even the [venerable co-author of Euterpea and the  Haskell School of Music](https://www.donyaquick.com/vivid-as-a-real-time-audio-solution-for-euterpea/) has sworn off attempting real-time in pure Haskell.

That said, I still think this is really interesting to work through for learning both Haskell and DSP. With some effort
in adding strictness annotations and representing state using simpler/even unboxed types, we can prevent space leaks, and
using wavetables for trigonometry functions may also help with computation speed.

The `Node` representation is a big step up from my previous attempts (4 years ago... ouch.) Before this I'd hopped around representations, first lazy infinite lists, which had free memoization by the runtime but were extremely allocation-heavy, then the simple `t -> x` reader monad which did not have statefulness that could compose. It's cool to have landed on something that seems to address these issues, though there will be definitely many more problems.

For any nontrivial audio processing, this will probably still need to be offline rather than real-time. That said, I'm doing my testing via SDL audio to play audio as the program runs.

## Organization
The library is in `src/`, and only depends on `mtl` at the moment. You can load the `Sound.Hailstone.Synth` module in GHCi or another Main to try it out directly, no `stack` or package installations needed.

The executable is in `exe/SDLTestMain.hs`, which is
where we write test songs and synths to play around with. It depends on `sdl2` and `vector`, and should probably be built using the `stack` instructions below.

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