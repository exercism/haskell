```haskell
responseFor :: String -> String
responseFor query
  | isSilent = "Fine. Be that way!"
  | isQuestion && isYelled = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isYelled = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isSilent = all isSpace query
    isQuestion = lastMay (filter (not . isSpace) query) == Just '?'
    isYelled = any isLetter query && not (any isLower query)
```

This solution uses `any` and `all` to determine whether the query consists entirely of whitespace, and whether all letters are uppercase.
It also eschews `last`, which is [partial][wiki-partial-functions], in favor of the safe alternative `lastMay`.


# Using dependencies

The function `lastMay` lives in the `Safe` module of the external `safe` package.
To be able to use it, you need to add this package to the list of dependencies in `package.yaml`:

```yaml
dependencies:
  - base
  - safe  # 👈 Add this line
```

Thereafter you can import functions as you would normally:

```haskell
import Safe (lastMay)
```


# `any` & `all`

`any` and `all` are **higher-order functions** that take a predicate (a function that produces a `Boolean`) and a list as arguments.
Both check whether elements of the list satisfy the predicate.
`any` produces `True` when there is _at least one_ element that satisfies the predicate, and `False` otherwise.
In contrast, `all` produces `True` only when _all_ elements satisfy the predicate.

```haskell
-- >>> any even [1, 3, 5]  -- no even numbers in this list
-- False
-- >>> any even [1 .. 5]   -- at least one even number in this list
-- True
-- >>> all even [2, 4, 6]  -- all numbers in this list are even
-- True
-- >>> all even [2 .. 6]   -- not all numbers in this list are even
-- False
```

How do these work?

~~~~exercism/advanced
To find they actual definitions of `any` and `all`, look up their documentation (for example through [Hoogle][hoogle]) and click on the «Source» link next to the type signature.

The definitions of `any` and `all` look kinda complicated!
They are this way so that they can also be used on types other than lists, such as `Set`, `Map`, and `Tree`.
Specifically, they can be used on any type that is an instance of the `Foldable` type class.

In the source code, you can click on names to jump to their definitions.
This doesn't really work for `foldMap` here though: it sends you to its default (general) implementation, but here we need its implementation for lists specifically.
To find that code, navigate to the documentation of `Foldable`, look up `[]` in the list of «Instances», and click «Source».

It might be hard to see &ndash; even after lots of clicking through to definitions &ndash; but these definitions of `any` and `all` work essentially the same way as the one outlined here below.

[hoogle]:
    https://hoogle.haskell.org/
    "Hoogle"
~~~~

Here is a possible definition of `any`:

```haskell
or :: [Bool] -> Bool
or = foldr (||) True

any p = or . map p
```

And this is how it evaluates:

```haskell
_ = any (2 <) [1..]
  == (or . map (2 <)) [1..]
  == or ( map (2 <) [1..] )
  == foldr (||) True ( map (2 <) [1..] )
     -- look at next element of the list
  == foldr (||) True ( 2 < 1 : map (2 <) [2..] )
  == 2 < 1  ||  foldr (||) True ( map (2 <) [2..] )
  == False  ||  foldr (||) True ( map (2 <) [2..] )
  == foldr (||) True ( map (2 <) [2..] )
     -- look at next element of the list
  == foldr (||) True ( 2 < 2 : map (2 <) [3..] )
  == 2 < 2  ||  foldr (||) True ( map (2 <) [3..] )
  == False  ||  foldr (||) True ( map (2 <) [3..] )
  == foldr (||) True ( map (2 <) [3..] )
     -- look at next element of the list
  == foldr (||) True ( 2 < 3 : map (2 <) [4..] )
  == 2 < 3  ||  foldr (||) True ( map (2 <) [4..] )
  == True   ||  foldr (||) True ( map (2 <) [4..] )
     -- (||) short-circuits
  == True
```

As you can see, evaluation terminates as soon as a number larger than `2` is found.
And thanks to laziness, `any` and `all` even work on infinite lists!
Provided the answer can be determined after looking at finitely many elements, that is.


# In this approach

A query is considered silent when it consists entirely of whitespace characters.
Which is to say: it is silent when _all_ of its characters are whitespace.
Hence,

```haskell
isSilent = all isSpace query
```

Similarly, a query is considered yelled when all its letters are uppercase, provided there is at least one letter.
This latter condition is expressed with `any`:

```haskell
atLeastOneLetterPresent = any isLetter query
```

That all letters should be uppercase is trickier to express.
`all isUpper query` would not do, as non-letters do not count as uppercase.

```haskell
-- >>> isUpper ' '
-- False
-- >>> isUpper '!'
-- False
-- >>> all isUpper "HI THERE!"
-- False
```

One way of working around this is filtering out non-letters first.

```haskell
-- >>> filter isLetter "HI THERE!"
-- "HITHERE"
-- >>> all isUpper (filter isLetter "HI THERE!")
-- True
```

Another is defining a combinator that combines predicates.

```haskell
implies :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p `implies` q = \x -> if p x then q x else True

-- >>> all (isLetter `implies` isUpper) "HI THERE!"
-- True
```

And yet another way is observing that if all letters are uppercase then there are no lowercase letters, and vice versa.

```haskell
-- >>> all (not . isLower) "HI THERE!"
-- True
-- >>> not (any isLower "HI THERE!")
-- True
```

`all (not . p) xs` and `not (any p xs)` are entirely equivalent, for all `p` and `xs`.

A query is considered a question when its last non-whitespace character is `'?'`.
To get at this character, the solution highlighted above first filters out all whitespace.
After this, it could have used `last`, but that function crashes when given an empty list:

```text
ghci> tail []
*** Exception: Prelude.tail: empty list
```

To avoid such crashes, the alternative `lastMay` function is used.
Instead of crashing, it will return `Nothing`.
And when the list is not empty `lastMay` returns a `Just`.

```haskell
-- >>> lastMay ""
-- Nothing
-- >>> lastMay "abc"
-- Just 'c'
```


[wiki-partial-functions]:
    https://en.wikibooks.org/wiki/Haskell/Variables_and_functions#where_clauses
    "Haskell Wikibook: where clauses"
