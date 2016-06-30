# xHaskell

[![Build Status](https://travis-ci.org/exercism/xhaskell.png?branch=master)](https://travis-ci.org/exercism/xhaskell)

Exercism exercises in Haskell

## Contributing Guide

Please see the [contributing guide](https://github.com/exercism/x-api/blob/master/CONTRIBUTING.md#the-exercise-data)

### Development Dependencies

There are currently two distinct ways to set up an environment to colaborate
with the development of the Haskell track:

- Using a global GHC installation.
- Using *Stack*.

The first method is more convenient when you just want to write code without
caring too much about which packages are being used. The second one is better
to track exercise's dependencies and test them against multiple GHC versions.

#### Using a global GHC installation

If you have a recent Haskell Platform installed, probably most of the packages
you need are already installed for you. Otherwise, you can manually install
the missing ones.

The following is the list of all packages used in this repository, just don't
trust this list to be updated.

These packages come installed with GHC:

- array
- base
- containers
- filepath
- directory
- process
- time
- unix

These are already installed in recent version of the Haskell Platform:

- attoparsec
- HUnit
- text
- parallel
- QuickCheck
- random
- split
- stm
- vector

These you will have to add to your instalation:

- lens
- old-locale

##### Installing missing packages

```bash
$ cabal install lens
```

This will download and installed the package named *lens*.


##### Running tests

All the tests:

```bash
$ ./_test/check-exercises.hs
-- accumulate
Cases: 5  Tried: 5  Errors: 0  Failures: 0
[…]
-- wordy
Cases: 16  Tried: 16  Errors: 0  Failures: 0
-- zipper
Cases: 8  Tried: 8  Errors: 0  Failures: 0
SUCCESS!
```

Test only specific exercises:

```bash
$ ./_test/check-exercises.hs triangle trinary
-- triangle
Cases: 8  Tried: 8  Errors: 0  Failures: 0
-- trinary
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
SUCCESS!
```

#### Using Stack

If you have stack installed, you can use it to handle all the dependencies
for you. But first, you need to transform the exercises in *stack projects*.

##### Creating a project for an exercise

The `_test/stackalize` bash script will extract the exercise's dependencies
from `_test/dependencies.txt` and create a *stack project* for you:

```bash
$ _test/stackalize --resolver lts-6.4 exercises/leap
```

This will transform the exercise *leap* in a *stack project* using the
resolver *lts-6.4*. Change it for your favourite [Stackage snapshot](https://www.stackage.org/snapshots).

*You can make you life easier adding _test to your path.*
That way you can simply call `stackalize` from anywhere instead of having
to provide a full path to `_test/stackalize`.

##### Testing with default settings

To download, install the compiler and packages needed, compile and test an
exercise, run the following commands:

```bash
$ cd exercises/leap
$ stack test
```

##### Testing with advanced options

Testing if it compiles without warnings and pass the tests:

```bash
$ stack clean
$ stack test --pedantic
```

Testing with a specific resolver:

```bash
$ stack test --resolver lts-2.22           # GHC-7.8.4
$ stack test --resolver lts-6.4            # GHC-7.10.3
$ stack test --resolver nightly-2016-06-21 # GHC-8.0.1
$ stack test --resolver nightly            # Newest compiler and packages.
```

If you are making major changes to an exercise, it's recommended that
you test it against those three versions of GHC with `--pedantic`, to be sure
it will not fail when your *pull request* is tested on *Travis-CI*.

##### Undoing the stack project

If you need to make a *commit*, you can remove the *stack project* and
change the exercise back to it's previous form:

```bash
$ stackalize --undo exercises/leap
```

This command will intentionally leave the *.stack-work* folder intact,
to preserve the cache in case you decide to test it with *stack* again.

#### The Exercises

Each exercise in the Haskell track is composed of at least two files:

- *name_test.hs*    # Both filenames must be all lowecase and any deviations
- *example.hs*      # will generate failures in the future.

Optionally, a third file with a stub can also be provided:

- *ModuleName.hs*   # This file must be named exactly as the module.

#### Tracking dependencies

We also keep track of all the packages used by the examples and test files in
*_test/dependencies.txt*. If you are implementing a new exercise or changing
the dependencies of a existing one, please don't forget to update it.

At the moment, we may not detect incorrectly specified dependencies in your
*pull requests* automatically. But soon we expect *Travis-CI* be able to
catch it, so please declare in *dependencies.txt* all the packages used by
the examples and tests.

## License

The MIT License (MIT)

Copyright (c) 2014 Katrina Owen, _@kytrinyx.com
