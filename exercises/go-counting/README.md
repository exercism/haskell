# Go Counting

Count the scored points on a Go board.

In the game of go (also known as baduk, igo, cờ vây and wéiqí) points
are gained by completely encircling empty intersections with your
stones. The encircled intersections of a player are known as its
territory.

Write a function that determines the territory of each player. You may
assume that any stones that have been stranded in enemy territory have
already been taken off the board.

Write a function that determines the territory which includes a specified coordinate.

Multiple empty intersections may be encircled at once and for encircling
only horizontal and vertical neighbours count. In the following diagram
the stones which matter are marked "O" and the stones that don't are
marked "I" (ignored).  Empty spaces represent empty intersections.

```text
+----+
|IOOI|
|O  O|
|O OI|
|IOI |
+----+
```

To be more precise an empty intersection is part of a player's territory
if all of its neighbours are either stones of that player or empty
intersections that are part of that player's territory.

For more information see
[wikipedia](https://en.wikipedia.org/wiki/Go_%28game%29) or [Sensei's
Library](http://senseis.xmp.net/).

## Hints

To complete this exercise, you need to implement the following functions:

- `territories` returns the coordinates (1 based, top left is (1,1))
of the points in each territory along with who "owns" the territory.

- `territoriesFor` returns the territory that contains the coordinate
along with the owner of the territory. If the coordinate does not point
to an empty location, returns *Nothing*.

A territory is owned by one of the players if that player's stones
are the only stones adjacent to the territory.

You will find the type signatures already in place, but it is up to you
to define the functions.



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
