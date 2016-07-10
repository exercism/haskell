
## Running Tests

###### If the exercise's folder looks like this...

```
./README.md
./exercise-name_test.hs
```

Execute the following command to run the tests:

```bash
stack runghc exercise-name_test.hs
```

###### Otherwise, it will be like this...

```
./README.md
./stack.yaml
./package.yaml
./src/ModuleName.hs
./test/Test.hs
```

In which case you should type:

```bash
stack test
```

## Solving the exercise

A solution to an exercise is a Haskell module. The module's name is defined
in an *import statement* in the test suite, usually at the beginning:

```haskell
import Control.Monad (unless)
import System.Exit   (exitFailure)

import Test.HUnit

import ModuleName (someFunc)

main :: IO ()
main ...
```

In this example, the test file imports, in line 6, a module named
`ModuleName`, that exports a function named `someFunc`. This means you have
to create a file named `ModuleName.hs` that would be something like this:

```haskell
module ModuleName (someFunc) where

someFunc :: ...
someFunc ...
```

If the exercise provides it, you'll a find a file with this name already
in place - in the same folder or in `src/` - which you can use as a starting
point for your solution.

Just keep in mind that this *stub*, if available, is there just for you
to get started. Feel free to change it completely if you think it is the
right thing to do.

#### Using additional packages

If you want to use additional packages to write a more elegant solution,
you'll need to install the packages or list them in `package.yaml`, depending
on the type of exercise you are solving.

###### Exercises without a *package.yaml* file

This will install packages `foo` and `bar` to your *implicit global project*:

```bash
stack install foo bar
```

###### Exercises with a *package.yaml* file

Just add the packages to your solution's dependencies in `package.yaml`:

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
