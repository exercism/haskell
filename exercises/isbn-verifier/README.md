# Isbn Verifier

Check if a given ISBN-10 is valid.

## Functionality

Given an unknown string the program should check if the provided string is a valid ISBN-10.
Putting this into place requires some thinking about preprocessing/parsing of the string prior to calculating the check digit for the ISBN.

The program should allow for ISBN-10 without the separating dashes to be verified as well.

## ISBN

Let's take a random ISBN-10 number, say `3-598-21508-8` for this.
The first digit block indicates the group where the ISBN belongs. Groups can consist of shared languages, geographic regions or countries. The leading '3' signals this ISBN is from a german speaking country.
The following number block is to identify the publisher. Since this is a three digit publisher number there is a 5 digit title number for this book.
The last digit in the ISBN is the check digit which is used to detect read errors.

The first 9 digits in the ISBN have to be between 0 and 9.
The check digit can additionally be an 'X' to allow 10 to be a valid check digit as well.

A valid ISBN-10 is calculated with this formula `(x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1) mod 11 == 0`
So for our example ISBN this means:
(3 * 10 + 5 * 9 + 9 * 8 + 8 * 7 + 2 * 6 + 1 * 5 + 5 * 4 + 0 * 3 + 8 * 2 + 8 * 1) mod 11 = 0

Which proves that the ISBN is valid.

## Caveats

Converting from string to number can be tricky in certain languages.
It's getting even trickier since the check-digit of an ISBN-10 can be 'X'.

## Bonus tasks

* Generate a valid ISBN-13 from the input ISBN-10 (and maybe verify it again with a derived verifier)

* Generate valid ISBN, maybe even from a given starting ISBN

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

Converting a string into a number and some basic processing utilizing a relatable real world example. [https://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digit_calculation](https://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digit_calculation)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
