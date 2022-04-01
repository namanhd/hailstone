# hailstone

haskell audio synthesis + song composition embedded languages, 
roughly in the spirit of `haskore` and the klangfreude microtonal 
tracker.

(Made mostly for learning purposes (Haskell and DSP), so the synthesis math/design might be shaky.)

## Synthesis
hailstone's audio synthesis uses the `Stream`, often called "sources" in the
code. A `Stream` is just an infinite lists of values whose values are calculated
lazily on-demand, but these values can be *functions*, which is where it gets
interesting. One can compose streams together such that parameters of a stream
of functions can themselves come from streams; this way, modulation interactions
between streams are obtained almost for free (in practice this is achieved
simply with `ZipList`-like `Functor` and `Applicative` instances for streams;
see `Data.Stream`)

For instance, one can define a sine source as follows:
```haskell
sineFn :: Double -> Freq -> Vol -> TimeVal -> Double
sineFn phase freq vol t = vol * sin (2 * pi * freq * t)

ts = timesteps sampleRate --create a stream of TimeVal sampled at sampleRate

-- A constant sinusoidal at 0 phase shift, frequency 440, volume 1.0:
sin440 = sineFn <$> pure 0.0 <*> pure 440.0 <*> pure 1.0 <*> ts
```

Then to modulate the frequency of another sine source using this
`sin440` stream:
```haskell
modulatedSinSource = sineFn <$> pure 0.0 <*> sin440 <*> pure 1.0 <*> ts
```

(`pure x` lifts an unboxed value `x` into a `Stream` that repeats `x` infinitely.)

In practice the `Applicative` infix operator notation is fine for quick math
operations on streams, but it might become a bit unwieldy for such a
commonly-used source like sine waves, so `Hailstone.Synth` defines the following
to quickly create a sine source from streams of parameters:
```haskell
sinSource :: Stream Double 
          -> Stream Freq 
          -> Stream Vol 
          -> Stream TimeVal 
          -> Stream Double
sinSource phases fs vs ts = sineFn <$> phases <*> fs <*> vs <*> ts
```
The above would thus become
```haskell
modulatedSinSource = sinSource (pure 0.0) sin440 (pure 1.0) ts
```

Streams can be combined by zipping them together using a function applied
pointwise (i.e. to corresponding values of each stream), as in the `Applicative`
instance. However, more powerfully, they can also be spliced up and sequenced
`piecewise`; one can define a stream to be composed of different streams that
play back in sequence, each with a given playback duration.

For reference, `piecewise`'s type is
```haskell
piecewise :: a  -- value to fill an empty stream when out of time
          -> TimeVal -- start time offset
          -> [(Stream a, TimeVal)] -- streams and their durations
          -> Stream TimeVal -- the time stream to work from
          -> Stream a
```

An expression like
```haskell
piecewise 0 0 [(sin440, 1.0), (modulatedSinSource, 2.0)] ts
```
will create a stream that plays the 440Hz sine for `1.0` second, then the
modulated sine wave (that we defined above) for `2.0` seconds, then silence (specified with `0`.) This is
in effect how a melody line can be coerced into `Stream` form; this is done with
the helper `notesToSource` which calls `piecewise` twice to create a stream for
frequencies and a stream for volumes. 

Playing back this melody using a carrier waveform stream (an "instrument") is
thus done by using this melody stream to modulate the frequency of the carrier
stream; the melody stream can also be used to modulate other things that must be
some function of the melody's frequencies. 

On the other hand, since melody frequencies are also just streams, they
themselves can also be modulated; this is how vibrato can be done (by adding a
sine stream onto the melody frequency stream). In the same spirit, since melody
volumes are also just streams, they can be modulated piecewise; this is how ADSR
and instrument envelopes can be done.

As seen above, composed stream functions take in a timestep stream, effectively
a quantized/sampled stream of time tick values of type `TimeVal`. This "samples"
the stream at the appropriate sample rate to be consumed by the audio backend
(which for now/for testing purposes is SDL audio, though the stream model should
be general enough to adapt to any buffer-consuming audio API).

## Composition

TODO not finished yet...

The composition language produces the note data to then convert into `Stream`
form.

In service of a free, unrestrictive, and unopinionated compositional paradigm,
there are no predefined note names. Composing a melody line consists of
stringing together consecutive intervals (which may be literal fractions/cent
values and variables assigned to these)

## Organization
The library is in `src/`. The executable is in `exe/Main.hs`, which is
where we write test songs and synths to play around with.

## Building
```
stack build
stack run
```
Requires SDL2 to be installed on the system.

Current `stack` resolver is `lts-16.22` (ghc 8.8.4).
However, this should work with newer ghcs and resolvers just fine.