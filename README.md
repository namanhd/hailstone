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
  MkNodeC :: s -> (SF a s (Either b x)) -> (Node a) -> (Node (b -> x)) -> Node x
  -- ^"conditional" node, modeling conditional eval of an input node (because on running the
  -- node tree, `MkNode2` strictly evaluates both child nodes unconditionally before it gets
  -- evaluated itself.)
```
> Note that the state type variable `s` does not show up on the left hand side of the data declaration. This makes `s` existentially quantified, i.e. `forall s. s -> ...`. Users of the `Node x` type cannot pattern match or know anything about the particular `s` type in use, only that it is compatible with the signal function `SF _ s x` it is bundled with.
>
> This makes sense, as implementations of audio nodes need not expose to the outside world what their internal state is or how it is used; different nodes in a node graph are sure to have different state needs internally, so it is only natural to hide `s` away rather than having it be parameterized in the datatype i.e. `Node s x`.
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
    go (MkNodeC s sig aNode fNode) = let
      (a, new_aNode) = go aNode
      (ebx, new_s) = runSF sig r a s
      (x, new_fNode) = case ebx of
        Right xDefault -> (xDefault, fNode)
        Left b -> let (f, new_fNode') = go fNode in (f b, new_fNode')
      in (x, MkNodeC new_s sig new_aNode new_fNode)
```


The comments in `Sound/Hailstone/Synth.hs` say more on the reasoning behind our choice of primitives/constructors and their evaluation semantics. However, this essentially lets us fairly directly translate object-oriented DSP code into our `Node` representation while preserving composability on the surface for users who use our provided nodes, combinators, and other functions.

> (There's also a nice Applicative instance we get almost for free with `MkNode2`, useful for lifting pure code into nodes. Unfortunately a Monad instance eludes me; I suspect it may not be doable in a sensible way because in some sense you need to "preallocate state" for each node in the tree before it's ever run, and monadic bind for the `Node` type implies "dynamically producing a new node tree structure on the fly during evaluation", which has caused many issues in my attempts. In any case, the applicative interface and the existing composability are more than expressive enough.)

For instance, here's a sine oscillator that holds its current angle (and a cached precomputed value) as state, translated nearly directly from https://juce.com/tutorials/tutorial_sine_synth/.

```haskell
sinOsc :: Node Freq -> Node Vol -> Node SynthVal
sinOsc = MkNode2 (0.0, Nothing) . sfr $ \(_, d) (f, vol) (myAngle, cachedDx2pi) -> let
  deltaTimes2pi = maybe (d * twopi) id cachedDx2pi
  angleDelta = f * deltaTimes2pi
  newAngle = myAngle + angleDelta
  newAngleNormed = if newAngle > twopi then newAngle - twopi else newAngle
  in (vol * sinFn myAngle, (newAngleNormed, Just deltaTimes2pi))
```

This is a function that takes two nodes (a frequency-emitting node and a volume-emitting node) and returns a stateful node that computes a sine wave, with state behind the scenes that the caller of this function  does not have to explicitly handle.

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
Nodes can be sequenced `piecewise`; one can define a node to be
composed of different nodes that play back in sequence, each with a given playback duration.

For reference, `piecewise`'s type is
```haskell
piecewise :: TimeVal -> a -> [(Node a, TimeVal)] -> Node a
```
An expression like
```haskell
piecewise 5.0 0.0 [(node1, 1.0), (node2, 2.0)]
```
will create a node that emits silence (the `0.0` argument) until time `t=5.0`, when it plays `node1` for `1.0` second, then `node2` for `2.0` seconds, then silence (`0.0`) again afterwards.

With `piecewise`, we can then define some nice audio/synthesis features:

- `retriggerWithNotes` restarts a synth node (any frequency and volume-parameterized node) on every new note, effectively
allowing one to "play a melody using a synth/instrument" by creating a keystroke
effect that retriggers and modulates the synth signal on every new note rather
than letting the synth sound evolve independently of the notes.
- `adsrEnvelope` together with `linearRamp` creates a signal that traverses an
Attack - Decay - Sustain - Release (ADSR) envelope, which can then be multiplied
by individual note-playing signals (for per-note ADSR), or entire summed signals
(for envelope-based modulation of volume, panning, even synthesis parameters
across playback).

## Composition
TODO!

## Issues and drawbacks

Performance. Most everyone has given up on getting end-to-end Haskell audio to be real-time capable; even the [venerable co-author of Euterpea and the  Haskell School of Music](https://www.donyaquick.com/vivid-as-a-real-time-audio-solution-for-euterpea/) has sworn off attempting real-time in pure Haskell.

That said, I still think this is really interesting to work through for learning both Haskell and DSP.

The `Node` representation is a big step up from my previous attempts (4 years ago... ouch.) Before this I'd hopped around representations, first lazy infinite lists, which had free memoization by the runtime but were extremely allocation-heavy, then the simple `t -> x` reader monad which did not have statefulness that could compose. It's cool to have landed on something that seems to address these issues, though there will be definitely many more problems. For any nontrivial audio processing, this will probably still need to be offline rather than real-time. That said, I'm doing my testing via SDL audio to play audio as the program runs.

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