# Palindrome Products

Detect palindrome products in a given range.

A palindromic number is a number that remains the same when its digits are
reversed. For example, `121` is a palindromic number but `112` is not.

Given the definition of a palindromic number, we define a palindrome _product_
to be the product `c`, such that `a * b = c`, where `c` is a palindromic number and
 `a` and `b` are integers (possibly, but _not_ necessarily palindromic numbers).

For example, the palindromic number 9009 can be written as the palindrome
product: `91 * 99 = 9009`.

It's possible (and indeed common) for a palindrome product to be the product
of multiple combinations of numbers. For example, the palindrome product `9` has
the factors `(1, 9)`, `(3, 3)`, and `(9, 1)`.

Write a program that given a range of integers, returns the smallest and largest
palindromic product within that range, along with all of it's factors.

## Example 1

Given the range `[1, 9]` (both inclusive)...

The smallest product is `1`. It's factors are `(1, 1)`.
The largest product is `9`. It's factors are `(1, 9)`, `(3, 3)`, and `(9, 1)`.

## Example 2

Given the range `[10, 99]` (both inclusive)...

The smallest palindrome product is `121`. It's factors are `(11, 11)`.
The largest palindrome product is `9009`. It's factors are `(91, 99)` and `(99, 91)`.

## Hints

To solve this exercise you need to implement these two functions:

- `largestPalindrome`
- `smallestPalindrome`

Both functions receive lower and upper factor limits, returning a pair
`(value, [(factor1, factor2)])` containing the palindrome and its possible
pairs of factors.

Your can use the provided signatures if you are unsure about the types, but
don't let them restrict your creativity.

It's ok to return duplicates in the factors list, and the order of the factors
is irrelevant.

You should consider using a slightly different algorithm to find small or
large palindromes.



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

Problem 4 at Project Euler [http://projecteuler.net/problem=4](http://projecteuler.net/problem=4)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
