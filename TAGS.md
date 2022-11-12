# Tags

This document aims to provide reasoning why `config.json` contains the `"tags"` it contains.

## Paradigms

- [x] `paradigm/declarative`: implied by `paradigm/functional`.
- [x] `paradigm/functional`: "purely functional" is Haskell's middle name.
- [ ] `paradigm/imperative`: even though Haskell is «the world's finest imperative programming language», the style is not prevalent. Unless you count `do`-notation.
- [ ] `paradigm/logic`: possible through DSL's, but not inherently supported. (Ignoring type class resolution.)
- [ ] `paradigm/object_oriented`: N/A
- [ ] `paradigm/procedural`: this would imply `paradigm/imperative`.

## Typing

- [x] `typing/static`: types are checked at compile time.
- [ ] `typing/dynamic`: types are erased at compile time and hence not available at run time.
- [x] `typing/strong`: as opposed to this term, types in Haskell are unambiguous.
- [ ] `typing/weak`: see `typing/strong`.

## Execution mode

- [x] `execution_mode/compiled`: this is the norm.
- [ ] `execution_mode/interpreted`: while possible (`runghc`), this is not the norm.

## Platform

- [x] `platform/windows`: entirely normal.
- [x] `platform/mac`: entirely normal.
- [x] `platform/linux`: entirely normal.
- [ ] `platform/ios`: unsure whether it has been done, so: no.
- [ ] `platform/android`: has been done, but is not the norm.
- [ ] `platform/web`: has been done, but is not the norm.

## Runtime

- [x] `runtime/standalone_executable`: the default.
- [ ] `runtime/language_specific`: 🤷 (of course it has &ndash; but is that notable?)
- [ ] `runtime/clr`: no support.
- [ ] `runtime/jvm`: no support.
- [ ] `runtime/beam`: no support.
- [ ] `runtime/wasmtime`: no support, but work is being done towards compiling Haskell to Wasm.

## Used for

- [ ] `used_for/artificial_intelligence`:
- [x] `used_for/backends`:
- [ ] `used_for/cross_platform_development`:
- [ ] `used_for/embedded_systems`: the large runtime and the garbage collection make this an ill fit.
- [x] `used_for/financial_systems`: Haskell is popular with banks and crypto (Cardano) folk.
- [ ] `used_for/frontends`:
- [ ] `used_for/games`:
- [ ] `used_for/guis`:
- [ ] `used_for/mobile`: too rarely.
- [ ] `used_for/robotics`:
- [ ] `used_for/scientific_calculations`: work is being done to make this nice, but it's not there yet.
- [ ] `used_for/scripts`: very possible, but uncommon.
- [ ] `used_for/web_development`:
