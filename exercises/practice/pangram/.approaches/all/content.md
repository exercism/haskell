# Express the definition of _pangram_

```haskell
isPangram :: String -> Bool
isPangram text = all (`elem` map toLower text) ['a' .. 'z']
```

This solution is simply the definition of _pangram_ put into ~~words~~ code.
A sentence is a pangram when `all` letters of the alphabet are present.

This approach iterates over the alphabet first and tolerates infinite input.
However, it is inefficient for some inputs.
This can be remedied using `Set`, at the cost of losing tolerance for infinite input.


## `any` & `all`

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

[hoogle]: https://hoogle.haskell.org/ "Hoogle"
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


## In this approach

We use `all` to check that all the letters of the alphabet are present in the input.

The predicate that we use is ``(`elem` map toLower text)``, which is an example of an [operator section][operator-section].
For any given letter, it checks that it is an element of the normalized input.

For some infinite inputs, such as `['a' .. 'z'] ++ repeat '!'`, this approach will be able to produce an answer in finite time.
If all letters are present, `elem` will find them all in finite time and `isPangram` will evaluate to `True`.
However, if any letter is absent, `elem` will never stop searching for it, and `isPangram` will never terminate.

`elem` performs a linear search: it checks elements one by one, in order.
Therefore, it can take a very long time to find values that occur only very deep into a list.
For example, for `replicate 1_000_000_000 '?' ++ ['x']` some 1.000.000.001 checks are required before it can be determined that `'x'` is an element of that list.

Having to look at many elements of very long lists is unavoidable.
However, this approach uses `elem` _26 times_, good for 26 separate linear searches, each of which might take long to complete.
This can certainly add up!

This is where `Set` comes in.
`fromList` allows gathering the entire input in one go.
Thereafter, checking for membership with `member` is very cheap, by the nature of `Set`s.

```haskell
import Data.Set (fromList, member)

isPangram :: String -> Bool
isPangram text = all (`member` characters) alphabet
  where
    alphabet = ['a' .. 'z']
    characters = fromList (map toLower text)
```

Because `fromList` needs to walk the list entirely before it can produce the `Set`, this version no longer works on infinite input.

Converting the alphabet to a `Set` wouldn't buy us improved performance.
There would be exactly the same number of elements either way.
However, it would allow us to use `isSubsetOf`, which is equivalent to the above use of `all` but might be preferred for aesthetic reasons.

```haskell
import Data.Set (fromDistinctAscList, fromList, isSubsetOf)

isPangram :: String -> Bool
isPangram text = alphabet `isSubsetOf` characters
  where
    alphabet = fromDistinctAscList ['a' .. 'z']
    characters = fromList (map toLower text)
```

Here `fromDistinctAscList` instead of `fromList` is used because it is more efficient, which is possible because `['a' .. 'z']` is already sorted and does not contain duplicates.
The argument of `isPangram` is not guaranteed to have these advantageous properties, so we must use `fromList`.


[operator-section]:
    https://wiki.haskell.org/Section_of_an_infix_operator
    "Haskell wiki: Section of an infix operator"
