## Hints

To complete this exercise you need to implement the function `slices`,
that takes a *text* and returns the subsequences of digits with a
specified size:

If it is your first time solving this exercise, it is recommended that you
stick to the provided signature:

```haskell
slices :: Int -> String -> [[Int]]
```

Later, it may be a good idea to revisit this problem and play with other data
types and libraries:

- `ByteString`, from package *bytestring*.
- `Sequence`, from package *containers*.
- `Text`, from package *text*.
- `Vector`, from package *vector*.

The test suite was intentionally designed to accept almost any type signature
that makes sense, so you are encouraged to find the one you think is the best.
