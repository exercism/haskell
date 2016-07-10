# xHaskell

[![Build Status](https://travis-ci.org/exercism/xhaskell.png?branch=master)](https://travis-ci.org/exercism/xhaskell)

Exercism exercises in Haskell

## Contributing Guide

Please see the [contributing guide](https://github.com/exercism/x-api/blob/master/CONTRIBUTING.md)

### Development Dependencies

You should have [Stack](http://docs.haskellstack.org/) installed in your
system to make contributing to this repository easier. Also, until we finish
migration of all exercises to stack projects
[#185](https://github.com/exercism/xhaskell/issues/185), you'll need some
additional packages installed in your *implicit global project*:

```bash
stack setup
stack install attoparsec HUnit lens old-locale parallel QuickCheck random split stm text vector
```

### Exercises

Currently, we have two types of exercises in this track:

###### Stack projects

We are migrating all the exercises to this new format:

- `stack.yaml` has just one line specifying the current
[Stack snapshot](https://www.stackage.org/snapshots). We use the same
resolver for all the exercises.
- `package.yaml` is a file in the [hpack](https://github.com/sol/hpack#readme)
format that has all dependencies and build instructions for an exercise.
- `src/Example.hs` is a sample solution passing the tests.
- `src/ModuleName.hs` is a *stub solution*.
- `test/Test.hs` is the test suite.

###### Legacy exercises

Legacy exercises have two or three files and no directories:

- `exercise-name_test.hs` is the tests suite.
- `example.hs` is a sample solution passing the tests.
- `ModuleName.hs` is an optional *stub solution*.

### Running Tests

###### Exercises that are stack projects

Rename the file `src/Example.hs` to match the module name and run:

```bash
stack test --pedantic
```

If the stub solution is still in the `/src` folder, build will probably fail.

###### Legacy exercises

Rename the file `example.hs` to match the module name and run:

```bash
stack runghc exercise-name_test.hs
```

## License

The MIT License (MIT)

Copyright (c) 2014 Katrina Owen, _@kytrinyx.com
