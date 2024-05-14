# Check that the alphabet is a substructure of the input

```haskell
isPangram :: String -> Bool
isPangram = (['a' .. 'z'] `isSubsequenceOf`) . sort . map toLower
```

This approach is to check that the alphabet is some kind of _substructure_ of the input.
What this should mean can be interpreted in various ways.
Three possible interpretations are described below.


## Interpretation: as a sequence of characters

A [subsequence][wikipedia-subsequence] is a sequence all of whose elements are present in another sequence, in the same order.
That is, a sequence is a subsequence if it is equal to another sequence with possibly some elements removed.

For example, «2, 3, 5» is a subsequence of «1, 2, 3, 4, 5», but «4, 2» is not.

```haskell
-- >>> [2, 3, 5] `isSubsequenceOf` [1 .. 5]
-- True
-- >>> [4, 2] `isSubsequenceOf` [1 .. 5]
-- False
```

As its name suggests, `isSubsequenceOf` checks that a list represents a subsequence of another list.
It does so efficiently &ndash; in one pass &ndash; and can even deal with infinite lists sometimes:

```haskell
-- >>> [5, 7 .. 20] `isSubsequenceOf` [0 ..]
-- True
-- >>> [0 ..] `isSubsequenceOf` [5, 7 .. 20]
-- False
```

It doesn't work when the finite list is _not_ a subsequence of the infinite one though.
Then `isSubsequenceOf` will never stop searching for the first value that is present on the left but not on the right.
That is, ``[0] `isSubsequenceOf` [1 ..]`` would never finish evaluating.

Similarly, it cannot deal with _two_ infinite lists.
You wouldn't be able to either: you would never be done searching for the next matching element!

~~~~exercism/advanced
To see `subsequence`'s source code, look up its documentation (for example through [Hoogle][hoogle]) and click on the «Source» link next to the type signature.

[hoogle]: https://hoogle.haskell.org/ "Hoogle"
~~~~

`isSubsequenceOf` can be used to check that all the letters of the alphabet are present in the input.
It cannot by used simply by itself though:

```haskell
alphabet = ['a' .. 'z']
text = reverse alphabet

-- >>> alphabet `isSubsequenceOf` text
-- False
```

The letters in the input should be put into the right order first.
This can be achieved with `sort`.

```haskell
-- >>> sort "cadbe"
-- "abcde"
```

Together, `isSubsequenceOf` and `sort` provide a solution to this exercise.

```haskell
isPangram :: String -> Bool
isPangram text = alphabet `isSubsequenceOf` characters
  where
    alphabet = ['a' .. 'z']
    characters = sort (map toLower text)
```

`isSubsequenceOf` would definitely be able to deal with infinitely long `text`, but `sort` cannot.
Hence, this solution does not tolerate infinite input.


## Interpretation: as a set of characters

A list either does contain a certain value, or it does not.
This exercise is all about characters being or not being present in strings.
This suggests viewing these strings as [sets][wikipedia-set-adt] of characters.
For this, the `Set` data type can be used.

~~~~exercism/note
The `Set` data type is not provided by `base`, the standard library.
Instead, it lives in the `containers` package.
To use it in your own solutions, add `containers` to the list of dependencies in the `package.yaml` file, like so:

```yaml
dependencies:
  - base
  - containers  # 👈
```
~~~~

Lists can be converted into `Set`s using the `fromList` function.

```haskell
someIntegers :: Set Integer
someIntegers = fromList [3, 3, 2, 3, 1, 2]

-- >>> elems someIntegers
-- [1,2,3]
```

`isSubsetOf` can be used to check that one `Set`'s elements are all present in another `Set`.

```haskell
s, t, r :: Set Integer
s = fromList [3, 5, 2]
t = fromList [1 .. 5]
r = fromList [6, 4]

-- >>> s `isSubsetOf` t
-- True
-- >>> r `isSubsetOf` t
-- False
```

This is precisely what is required in this exercise.

```haskell
isPangram :: String -> Bool
isPangram text = alphabet `isSubsetOf` characters
  where
    alphabet = fromDistinctAscList ['a' .. 'z']
    characters = fromList (map toLower text)
```

Here `fromDistinctAscList` instead of `fromList` is used because it is more efficient, which is possible because `['a' .. 'z']` is already sorted and does not contain duplicates.
The argument of `isPangram` is not guaranteed to have these advantageous properties, so we must use `fromList`.


## Interpretation: as a multiset of characters

Where a set is concerned with _whether_ a certain value is present, a [multiset][wikipedia-multiset-adt] is concerned with _how often_ it is present.

The `MultiSet` data type is very similar to `Set`, but is more general in that it keeps track of how many copies of the same value it contains.
In a sense, a `Set` is just a `MultiSet` in which every value occurs at most once.

~~~~exercism/note
`MultiSet` lives in the `multiset` package, which you need to add to the list of dependencies in `package.yaml` in order to use it.
~~~~

~~~~exercism/note
In other language, multisets are also known as `Bag`s, and as `Counter` in Python.
~~~~

```haskell
letters :: MultiSet Char
letters = fromList "abbcccdddd"

-- >>> toOccurList letters
-- [('a',1),('b',2),('c',3),('d',4)]
```

For this exercise, `MultiSet`'s extra capabilities aren't useful.
However, in other situations they often are, and multisets are a useful tool to have in your toolbox.

Like `Set`s, `MultiSet`s have a notion of _subset_.
A multiset is a subset when all its elements are present at least as often in another multiset.

```haskell
s, t :: MultiSet Char
s = fromList "a bb ccc dddd"
t = fromList "dddddd aa cccc ee bb"

-- >>> s `isSubsetOf` t
-- True
-- >>> t `isSubsetOf` s
-- False
```

Again, this is precisely what is required in this exercise: a phrase is a pangram if it contains every letter at least as often as the alphabet.

```haskell
isPangram :: String -> Bool
isPangram text = alphabet `isSubsetOf` characters
  where
    alphabet = fromDistinctAscList ['a' .. 'z']
    characters = fromList (map toLower text)
```

The code is the same as for the above version using `Set`, because none of `MultiSet`'s extra capabilities are needed.


[wikipedia-multiset-adt]:
    https://en.wikipedia.org/wiki/Set_(abstract_data_type)#Multiset
    "Wikipedia: Multiset (abstract data type)"
[wikipedia-set-adt]:
    https://en.wikipedia.org/wiki/Set_(abstract_data_type)
    "Wikipedia: Set (abstract data type)"
[wikipedia-subsequence]:
    https://en.wikipedia.org/wiki/Subsequence
    "Wikipedia: Subsequence"
