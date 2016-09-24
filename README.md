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

Rename the file `src/Example.hs` to match the module name and run:

```bash
stack test --pedantic
```

If the stub solution is still in the `/src` folder, build will probably fail.

## License

The MIT License (MIT)

Copyright (c) 2014 Katrina Owen, _@kytrinyx.com

### Haskell icon
The Haskell icon was designed by Darrin Thompson and Jeff Wheeler. It was released under the [HaskellWiki license](https://wiki.haskell.org/HaskellWiki:Copyrights).
