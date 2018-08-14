
## Running Tests

Execute the following command in your exercise directory to run the tests:

```bash
stack test
```

To run the tests on every file save, run this command instead:

```bash
stack test --file-watch
```

#### If you get an error message like this...

```
No .cabal file found in directory
```

You are probably running an old stack version and need
to upgrade it.

#### Otherwise, if you get an error message like this...

```
No compiler found, expected minor version match with...
Try running "stack setup" to install the correct GHC...
```

Just do as it says and it will download and install
the correct compiler version:

```bash
stack setup
```

## Solving the exercise

A solution to an exercise is a Haskell module. The module's name is defined
in an *import statement* in the test suite, usually at the beginning:

```haskell
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ModuleName (someFunc)

main :: IO ()
main ...
```

In this example, the test file imports, in line 4, a module named
`ModuleName`, that exports a function named `someFunc`. This means you have
to create a file named `ModuleName.hs` that would be something like this:

```haskell
module ModuleName (someFunc) where

someFunc :: ...
someFunc ...
```

You'll a find a file with this name already in place in `src/`,
which you can use as a starting point for your solution.

Just keep in mind that this *stub* is there just for you to get started.
Feel free to change it completely if you think it is the right thing to do.

#### Using packages

If you want to use some packages to write a more elegant solution, just
add the packages to your solution's dependencies in `package.yaml`:

```yaml
library:
  exposed-modules: ModuleName
  source-dirs: src
  dependencies:
    - foo
    - bar
```

#### Running *GHCi*

If you want to play with your solution in GHCi, just run the command:

```bash
stack ghci
```

## HLint (optional)

It's highly recommended that you run `hlint` on your solution to see if it
catches something you are missing:

```bash
hlint ModuleName.hs
```

You don't have to accept all the suggestions given by `hlint`, but usually
they will help you learn how to write beautiful, idiomatic Haskell code.
