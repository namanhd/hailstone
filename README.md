# hailstone

haskell audio synthesis + song composition embedded languages

(Made mostly for learning purposes (Haskell and DSP), so the synthesis
math/design are decidedly janky; see [Issues and
drawbacks](#issues-and-drawbacks))

## Synthesis
At the moment a signal is modeled simply as a function that depends on the
current time (or for digital audio which is discretized: time, sample rate (in
practice, `1/sampleRate` which gives the delta time between sampling time
ticks), and the current stereo channel). This can be described with the type
```haskell
type Signal a = (TimeVal, TimeVal, CurrChan) -> a
```
but this is just the same type as the `Reader` monad, where the reader context
is of type `(TimeVal, TimeVal, CurrChan)`, so we can just say
```haskell
type Signal = Reader (TimeVal, TimeVal, CurrChan)
```
and we get the niceties of the reader monad for free. (This simple, stateless,
time-dependent-only formulation doesn't support truly correct and robust
oscillators---see [stateful signals](#stateful-signals) for more on this, a
solution for which I have yet to fully work out.)

This means defining signal functions looks like this (where `getTime :: Signal
TimeVal` is a monadic action that extracts the current time from the
reader context):
```haskell
-- sin waveform function
sinewave :: TimeVal -> SynthVal -> Freq -> Vol -> SynthVal
sinewave t phase freq vol = vol * sin (2 * pi * freq * t)

-- A constant sinusoidal signal at 0 phase shift, frequency 440, volume 1.0:
sin440 :: Signal SynthVal
sin440 = sinewave <$> getTime <*> pure 0.0 <*> pure 440.0 <*> pure 1.0
```

Then to modulate the frequency of another sine source using this
`sin440` stream:
```haskell
modulatedSinSource = sinewave <$> getTime <*> pure 0.0 <*> sin440 <*> pure 1.0
```

Signals can also be sequenced `piecewise`; one can define a signal to be
composed of different signals that play back in sequence, each with a given
playback duration.

For reference, `piecewise`'s type is
```haskell
piecewise :: a -> [(Signal a, TimeVal)] -> Signal a
```
An expression like
```haskell
piecewise 0.0 [(sin440, 1.0), (modulatedSinSource, 2.0)]
```
will create a signal that plays the 440Hz sine for `1.0` second, then the
modulated sine wave (defined above) for `2.0` seconds, then silence
(the value specified with the first (`0.0`) argument.)

With `piecewise`, we can then define some nice audio/synthesis features:

- `retriggerWithNotes` restarts the synth signal on every new note, effectively
allowing one to "play a melody using a synth/instrument" by creating a keystroke
effect that retriggers and modulates the synth signal on every new note rather
than letting the synth sound evolve independently of the notes.
- `adsrEnvelope` together with `linearRamp` creates a signal that traverses an
Attack - Decay - Sustain - Release (ADSR) envelope, which can then be multiplied
by individual note-playing signals (for per-note ADSR), or entire summed signals
(for envelope-based modulation of volume, panning, even synthesis parameters
across playback).

## Composition

TODO not finished yet...

The composition language produces the note data to then convert into `Signal`
form.

In service of a free, unrestrictive, and unopinionated compositional paradigm,
there are no predefined note names. Composing a melody line consists of
stringing together consecutive intervals (which may be literal fractions/cent
values and variables assigned to these)

## Issues and drawbacks
The original signal model in Hailstone was based on lazy infinite lists, which
are idiomatic in Haskell and pleasant to program with. However, this
came with the huge drawback of having too many list-node allocations, consuming
large amounts of memory and CPU time, and yielding overall terrible performance
(the audio lags with just a few oscillators in 5 voices in even a high buffer
size like 8192.) (See the code for this in `SynthOld.hs`)

The current approach uses the reader monad, which means building up signals
effectively boils down to a composition of time-dependent functions awaiting the
application of the `(time, delta, channel)` reader context argument, which is
only applied at the very end when the buffer needs to be written to. This is
nice enough: there are almost no allocations involved other than the index list
at the very end for the buffer-writing, and is a lot faster, yielding smooth
audio at 44100Hz sample rate and a buffer size of 2048 (and maybe even 1024).

However, the one thing that the lazy list approach had that this doesn't is that
once computed, list nodes (i.e. generated samples) are automatically memoized by
the runtime. This allows the convolution functions (which require
computation of the signal at time values in advance of the current time) to
incur only a negligible performance hit since subsequent values can just be
retrieved from the memoization. In contrast, in the reader monad approach, this
kind of naive convolution results in massive performance loss and unworkable
audio, since there is no memoization and the signal is effectively recalculated
`n` times where `n` is the convolution window size.

### Stateful signals
The other huge disadvantage with this approach (that I haven't been able to work
out nicely) is how to do *stateful* signals. To smoothly modulate the
frequency of an oscillator (for something like portamento), simply inserting
`freq = freqfunc(t)` as in e.g. `sin (2*pi*freqfunc(t)*t)` isn't going to work,
as the resulting intermediate sine waves would be discontinuous and the
intermediate waves not correct. Statefulness would also be a solution to
filtering as above, since we would only need to keep track of a running average.

My attempts at implementing this so far have blown up spectacularly; the natural
thing to do is to replace the Reader monad with a State monad, but then each
oscillator in each voice would need its own state value; and even naively
replacing Reader with State with no functionality change already slows down the
signal pipeline a whole lot, so something better and more clever must be used...

## Organization
The library is in `src/`. The executable is in `exe/Main.hs`, which is
where we write test songs and synths to play around with.

## Building
```
stack build
stack run
```
Requires SDL2 to be installed on the system. (also, the included `stack.yaml`
uses the system ghc by default, which you might want to change depending on the
config)

Current `stack` resolver is `lts-16.22` (ghc 8.8.4).
However, this should work with newer ghcs and resolvers just fine.