# Custom Set

Create a custom set type.

Sometimes it is necessary to define a custom data structure of some
type, like a set. In this exercise you will define your own set. How it
works internally doesn't matter, as long as it behaves like a set of
unique elements.

## Hints

To complete this exercise, you need to create the data type `CustomSet`,
with `Eq` and `Show` instances, and implement the following functions:

- `delete`
- `difference`
- `empty`
- `fromList`
- `insert`
- `intersection`
- `isDisjointFrom`
- `isSubsetOf`
- `member`
- `null`
- `size`
- `toList`
- `union`

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.

If you're interested in writing an efficient implementation but don't quite
know where to start, the best primer I know of is Chris Okasaki's
"Purely Functional Data Structures", which you can read a version of here:
https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf



## Getting Started

For installation and learning resources, refer to the
[exercism help page](http://exercism.io/languages/haskell).

## Running the tests

To run the test suite, execute the following command:

```bash
stack test
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

## Running *GHCi*

If you want to play with your solution in GHCi, just run the command:

```bash
stack ghci
```

## Feedback, Issues, Pull Requests

The [exercism/haskell](https://github.com/exercism/haskell) repository on
GitHub is the home for all of the Haskell exercises.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
