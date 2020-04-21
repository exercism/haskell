# Clock

Implement a clock that handles times without dates.

You should be able to add and subtract minutes to it.

Two clocks that represent the same time should be equal to each other.

## Hints

It's a 24 hour clock going from "00:00" to "23:59".

To complete this exercise you need to define the data type `Clock`,
add an `Eq` instance, and implement the functions:

- addDelta
- fromHourMin
- toString

`addDelta` adds a duration, expressed in hours and minutes, to a given time,
represented by an instance of `Clock`.

`fromHourMin` takes an hour and minute, and returns an instance of `Clock` with 
those hours and minutes.

`toString` takes an instance of `Clock` and returns a string representation 
of the clock, in 0-padded format like "08:03" or "22:35"

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.

If you need help getting started with types, take a look at:
- [Data Types in 5 Steps][types]

[types]: https://mmhaskell.com/blog/2017/12/24/haskell-data-types-in-5-steps



## Getting Started

Please refer to the [installation](https://exercism.io/tracks/haskell/installation)
and [learning](https://exercism.io/tracks/haskell/learning) help pages.

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

## Source

Pairing session with Erin Drummond [https://twitter.com/ebdrummond](https://twitter.com/ebdrummond)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
