# Hints

To complete this exercise you need to implement the function `anagramsFor`,
that takes a *word* and a group of *words*, returning the ones that are
anagrams of the given *word*.

If it is your first time solving this exercise, it is recommended that you
stick to the provided signature:

```haskell
anagramsFor :: String -> [String] -> [String]
```

Later, it may be a good idea to revisit this problem and play with other data
types and libraries:

- `Text`, from package *text*.
- `Sequence` and `Set`, from package *containers*.
- `MultiSet`, from package *multiset*

The test suite was intentionally designed to accept almost any type signature
that makes sense, so you are encouraged to find the one you think is the best.
