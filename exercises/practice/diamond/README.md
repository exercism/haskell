# Diamond

The diamond kata takes as its input a letter, and outputs it in a diamond
shape. Given a letter, it prints a diamond starting with 'A', with the
supplied letter at the widest point.

## Requirements

* The first row contains one 'A'.
* The last row contains one 'A'.
* All rows, except the first and last, have exactly two identical letters.
* All rows have as many trailing spaces as leading spaces. (This might be 0).
* The diamond is horizontally symmetric.
* The diamond is vertically symmetric.
* The diamond has a square shape (width equals height).
* The letters form a diamond shape.
* The top half has the letters in ascending order.
* The bottom half has the letters in descending order.
* The four corners (containing the spaces) are triangles.

## Examples

In the following examples, spaces are indicated by `·` characters.

Diamond for letter 'A':

```text
A
```

Diamond for letter 'C':

```text
··A··
·B·B·
C···C
·B·B·
··A··
```

Diamond for letter 'E':

```text
····A····
···B·B···
··C···C··
·D·····D·
E·······E
·D·····D·
··C···C··
···B·B···
····A····
```

## Hints

You need to implement the `diamond` function which prints a diamond starting at
`A` with the given character at its widest points. You can use the provided
signature if you are unsure about the types, but don't let it restrict your
creativity:

```haskell
diamond :: Char -> Maybe [String]
```

This exercise works with textual data. For historical reasons, Haskell's
`String` type is synonymous with `[Char]`, a list of characters. For more
efficient handling of textual data, the `Text` type can be used.

As an optional extension to this exercise, you can

- Read about [string types](https://haskell-lang.org/tutorial/string-types) in
  Haskell.
- Add `- text` to your list of dependencies in package.yaml.
- Import `Data.Text` in [the following
  way](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c):

```haskell
import qualified Data.Text as T
import           Data.Text (Text)
```

- You can now write e.g. `diamond :: Char -> Maybe [Text]` and refer to
  `Data.Text` combinators as e.g. `T.pack`,
- Look up the documentation for
  [`Data.Text`](https://hackage.haskell.org/package/text/docs/Data-Text.html),
- You can then replace all occurrences of `String` with `Text` in Diamond.hs:

```haskell
diamond :: Char -> Maybe [Text]
```

This part is entirely optional.



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

Seb Rose [http://claysnow.co.uk/recycling-tests-in-tdd/](http://claysnow.co.uk/recycling-tests-in-tdd/)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
