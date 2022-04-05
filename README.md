# hailstone

haskell audio synthesis + song composition embedded languages, 
roughly in the spirit of `haskore` and the klangfreude microtonal 
tracker.

(Made mostly for learning purposes (Haskell and DSP), so the synthesis
math/design might be shaky; see [Issues and drawbacks](#issues-and-drawbacks))

## Synthesis
hailstone's audio synthesis revolves around `Stream`s, often called "sources" in
the code. A `Stream` is just an infinite lists of values whose values are
calculated lazily on-demand. One can compose streams together and use them as
streams of arguments for functions (e.g. oscillators); this way, modulation
interactions between oscillators are obtained almost for free.

For instance, one can define a sine source as follows.
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

(`pure x` lifts an unboxed value `x` into a `Stream` that repeats `x`.)

In practice the `Applicative` infix operator notation is fine for quick math
operations on streams, but due to the way `(<*>)` is defined in terms of 2-ary
`zipWith`, the intermediate streams of partially-applied functions can quickly
build up and cause lots of allocations. This will result in general slowness and
badness with math expressions using `(<*>)`. So, `sinSource` and similar
functions are defined as the following to quickly and efficiently create a sine
source from parameters:
```haskell
sinSource :: Stream Double 
          -> Stream Freq 
          -> Stream Vol 
          -> Stream TimeVal 
          -> Stream Double
-- In spirit: sinSource phases fs vs ts = sineFn <$> phases <*> fs <*> vs <*> ts
sinSource = zipWith4 sineFn
```
The above would thus become
```haskell
modulatedSinSource = sinSource (pure 0.0) sin440 (pure 1.0) ts
```

So, `Stream`s can be combined by zipping them together using a function applied
pointwise. However, more powerfully, they can also be spliced up and sequenced
`piecewise`; one can define a stream to be composed of different streams that
play back in sequence, each with a given playback duration.

For reference, `piecewise`'s type is
```haskell
piecewise :: ChanMode -- Mono or Stereo
          -> a  -- value to fill an empty stream when out of time
          -> TimeVal -- start time offset
          -> [(Stream a, TimeVal)] -- streams and their durations
          -> Stream TimeVal -- the time stream to work from
          -> Stream a
```

An expression like
```haskell
piecewise Mono 0 0 [(sin440, 1.0), (modulatedSinSource, 2.0)] ts
```
will create a stream that plays the 440Hz sine for `1.0` second, then the
modulated sine wave (that we defined above) for `2.0` seconds, then silence
(specified with the first `0` argument.) This is in effect how a melody line can
be coerced into `Stream` form; this is done with the helper `notesToStreams`
which calls `piecewise` three times to create a stream for frequencies, a stream
for volumes, and a stream for panning values (because note cells may also store
their own pan values.)

With `piecewise`, we can "play back" a melody using a carrier waveform stream
(an "instrument") by using this melody stream to modulate the frequency of the
carrier stream. The melody stream can also be used to modulate other things that
must be some function of the melody's frequencies.

Alternatively, `retriggerWithNotes` restarts the synth signal on every new note,
creating a "keystroke/retrigger" effect, rather than letting the synth sound
continue to evolve independently of the melody.

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

## Issues and drawbacks
Real-time audio synthesis in Haskell has been attempted by many, and it does not
appear to be a solved problem at all. While the infinite lazy list interface
might be extremely idiomatic, expressive, and straightforward, it's also really
slow and thus not truly adequate for real-time audio. (Having less than 10 sine
sources in the test application already sometimes lags the audio even with large
buffer sizes like `4096`.) 

The best crack at this hard-realtime problem has probably been Henning
Thielemann's (also developer of `haskore`) `synthesizer` package, and 
[weighing many options and difficulties](https://wiki.haskell.org/Synthesizer), 
that package ended up doing its audio streams by 
[constructing LLVM code at runtime](https://haskell-cafe.haskell.narkive.com/MM57Tz5H/lazy-cons-stream-fusion-style); 
a far cry from the raw elegance (i.e. low mental and keystroke load) of the lazy
list approach.

Since hailstone is a hobby learning project, I will probably not aim for
industrial-strength audio performance, even if a more full-fledged GUI music
editor evolves around this. The elegant synthesis paradigm using lazy lists
(what I call the `Stream`) is too hard to give up (at least, at the moment),
given that it is only a problem at realtime and not render-time, and given that
for now most of the composing work done on this will be as an embedded DSL.

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