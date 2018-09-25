# Robot Name

Manage robot factory settings.

When robots come off the factory floor, they have no name.

The first time you boot them up, a random name is generated in the format
of two uppercase letters followed by three digits, such as RX837 or BC811.

Every once in a while we need to reset a robot to its factory settings,
which means that their name gets wiped. The next time you ask, it will
respond with a new random name.

The names must be random: they should not follow a predictable sequence.
Random names means a risk of collisions. Your solution must ensure that
every existing robot has a unique name.

## Hints

To complete this exercise, you need to create the data type `Robot`,
as a mutable variable, and the data type `RunState`. You also need to
implement the following functions:

- `initialState`
- `mkRobot`
- `resetName`
- `robotName`

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym. To model state this exercise uses the
[`State`](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
monad. More specifically we combine the `State` monad with the `IO` monad using
the [`StateT` monad transfomers](http://book.realworldhaskell.org/read/monad-transformers.html).
All tests are run with `initialState` as the state fed into to `evalStateT`.



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

## Source

A debugging session with Paul Blackwell at gSchool. [http://gschool.it](http://gschool.it)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
