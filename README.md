# xHaskell

[![Build Status](https://travis-ci.org/exercism/xhaskell.png?branch=master)](https://travis-ci.org/exercism/xhaskell)

Exercism exercises in Haskell

## Contributing Guide

Please see the [contributing guide](https://github.com/exercism/x-api/blob/master/CONTRIBUTING.md#the-exercise-data)

### Development Dependencies

Currently, development of the Haskell track assumes that you are using
GHC 7.8.3 with Haskell Platform installed. The following packages need
to be installed if not using Haskell Platform:

```bash
$ comm -13 \
  <(ghc-pkg --global list --simple-output --names-only \
  | tr ' ' '\n') \
  <(find . -name '*.hs' \
  | xargs awk \
    '/^import/ {if ($2 == "qualified") {print $3} else {print $2}}' \
  | sort -u \
  | xargs -n1 ghc-pkg find-module --simple-output --names-only \
  | cut -d' ' -f1 \
  | sort -u)
HUnit
QuickCheck
attoparsec
parallel
random
regex-base
regex-compat
split
stm
text
vector
```

### Running tests

All the tests:

```bash
$./_test/check-exercises.hs
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

## License

The MIT License (MIT)

Copyright (c) 2014 Katrina Owen, _@kytrinyx.com
