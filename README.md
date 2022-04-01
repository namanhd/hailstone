# hailstone

haskell audio synthesis + song composition embedded languages, 
roughly in the spirit of `haskore` and the klangfreude microtonal 
tracker.

## Synthesis
The main driver of hailstone's audio synthesis is the `Stream`, often called
"sources" in the code. Streams are just infinite lists of values (whose values
are calculated lazily on-demand; bless Haskell :>), but these values can be
*functions*, which is where it gets very spicy. One can compose streams together
such that parameters of a stream of functions can themselves come from streams;
this way, modulation interactions between streams are obtained for free.

Streams can be combined by zipping them together using a function applied
pointwise (i.e. to corresponding values of each stream). However, more
powerfully, they can also be spliced up and sequenced `piecewise`; one can
define a stream to be composed of different streams that play back in sequence,
each with a given playback duration. 

This is in effect how a melody line can be coerced into `Stream` form. Playing
back this melody using a carrier waveform stream (an "instrument") is thus done
by using this melody stream to modulate the frequency of the carrier stream; the
melody stream can also be used to modulate other things that must be some
function of the melody's frequencies. 

On the other hand, since melody frequencies are also just streams, they
themselves can also be modulated; this is how vibrato can be done (by adding a
sine stream onto the melody frequency stream).

Finally, composed stream functions take in a timestep stream, effectively a
quantized/sampled stream of time tick values of type `TimeVal`. This "renders"
the stream into PCM bytes at the appropriate sample rate to be consumed by the
audio backend (which for now/for testing purposes is SDL audio, though the
stream model should be general enough to adapt to any buffer-consuming audio
API).

## Composition

TODO not finished yet...

The composition language produces the note data to then convert into `Stream`
form.

In service of a free, unrestrictive, and unopinionated compositional paradigm,
there are no predefined note names. Composing a melody line consists of
stringing together consecutive intervals (which may be literal fractions/cent
values, or elsewhere custom-defined values like those.)

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