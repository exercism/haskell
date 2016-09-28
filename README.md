# xHaskell

[![Build Status](https://travis-ci.org/exercism/xhaskell.png?branch=master)](https://travis-ci.org/exercism/xhaskell)

Exercism exercises in Haskell

## Contributing Guide

Please see the [contributing guide](https://github.com/exercism/x-api/blob/master/CONTRIBUTING.md)

### Development Dependencies

You should have [Stack](http://docs.haskellstack.org/) installed in your
system to make contributing to this repository easier.

### Exercises

All exercises have the following structure:

- `stack.yaml` has just one line specifying the current
[Stack snapshot](https://www.stackage.org/snapshots). We use the same
resolver for all the exercises.
- `package.yaml` is a file in the [hpack](https://github.com/sol/hpack#readme)
format that has all dependencies and build instructions for an exercise.
- `src/Example.hs` is a sample solution passing the tests.
- `src/ModuleName.hs` is a *stub solution*.
- `test/Tests.hs` is the test suite.
- `HINTS.md` is an optional file containing instructions and/or hints.

### Running Tests

In order to be accepted by Travis-CI, every exercise must compile without
warnings and the example solution must pass the tests without failures.

To test an exercise, first you need to rename the file `src/Example.hs`
to match the module name, but - because we already have a *stub solution*
in place - we have to move it first, otherwise it will be overwritten.

Running `stack test --pedantic` compiles and run the tests with
`-Wall -Werror`, but unfortunately it doesn't recompile unchanged
source code already compiled with warnings.

To be really sure that everything compiles correctly without
warnings, your must first run `stack clean`.

```bash
stack clean
stack test --pedantic
```

### Running HLint

All code in this repository should be as idiomatic as possible, so we
enforce in [Travis-CI] that it returns `No hints` when processed by
HLint.

It is highly recommended to run `hlint` on your sources before opening
a *pull request*, so you can fix your code before submitting it for review.

If you are certain that a suggestion given by `hlint` would make the
code worse, you can [suppress it](https://github.com/ndmitchell/hlint#customizing-the-hints)
with annotations in the source file.

## License

The MIT License (MIT)

Copyright (c) 2014 Katrina Owen, _@kytrinyx.com

### Haskell icon
The Haskell icon was designed by Darrin Thompson and Jeff Wheeler. It was released under the [HaskellWiki license](https://wiki.haskell.org/HaskellWiki:Copyrights).
