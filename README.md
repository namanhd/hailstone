# hailstone

***ha**skell **i**ntegrated **l**anguages for **s**ynthesizing **tone**s*

Audio synthesis + song composition embedded languages.

## Design
See [DESIGN.md](DESIGN.md) for a walkthrough of the synthesis architecture and the audio backend.

## Organization
The library is in `src/`, and has no dependencies outside of the libraries included with GHC (`array` for the core synthesis; `text`, `mtl` for the sequencing DSL). You can load library modules in GHCi or another Main to try it out directly, no external package installations needed.

The executable is in `exe/ScratchMain.hs`, which is where we write test songs and synths to
play around with. It depends on `portaudio` and `array` and should be built as a project,
using the .cabal file.

## Writing music

Right now using the embedded DSLs in `ScratchMain.hs` and compiling and running the program is the only way to write and play music using hailstone. I'll be working on a standalone interpreter and a GUI program next, hopefully.

## Building
```
cabal build
cabal run
```
PortAudio is used to play real-time audio. Requires PortAudio to be installed on the system (steps vary: on Arch: `pacman -S portaudio`, Ubuntu: `apt install portaudio19-dev libportaudio2`)

**Requires GHC >=9.12.2** (for SIMD math with the native code generator. Older GHCs may work
with `-fllvm`, but I haven't tested this with the LLVM codegen.)

This should probably work with newer ghcs fine.

## Plans
- [x] Node-based real-time audio system
- [x] Embedded node graph definition DSL (came with the design of the node system)
- [x] Embedded composition DSL (`Sound.Hailstone.Sequencing.CellScript`)
- [ ] Embedded envelope/automation sequencing DSL (perhaps `Sound.Hailstone.Sequencing.NodeScript`)
- [ ] Standalone hailstone interpreter and language for node graph definition and composition
  - This will let users write songs in files that are interpreted by the hailstone executable, without requiring a GHC and Haskell toolchain.
- [ ] GUI program for node graph definition and composition (a DAW!)