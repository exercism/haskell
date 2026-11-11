# Validate first, then transcribe

```haskell
toRNA :: String -> Either Char String
toRNA dna =
  case find (`notElem` "GCTA") dna of
    Nothing -> Right (map transcribe dna)
    Just c -> Left c
  where
    transcribe = \case
      'G' -> 'C'
      'C' -> 'G'
      'T' -> 'A'
      'A' -> 'U'
```

One approach to solving this problem is to

- first check that all input characters are valid,
- return one of the invalid characters if there are any, and otherwise to
- convert all the DNA nucleotides into RNA nucleotides.

Some submitted solutions retrieve the invalid character (if present) in two steps:

- first check that there are _some_ invalid characters, for example using `any`, and
- then find the first one, for example using `filter` and `head`.

The solution highlighted here combines these steps into one.
As used here, `find` returns `Nothing` if there are no invalid characters, and if there are then it returns `Just` the first one.
By pattern matching on `find`'s result it is determined how to proceed.

For transcribing DNA nucleobases into RNA nucleobases a locally defined function `transcribe` is used.
It is a [partial function][wiki-partial-functions]: when given any character other than `'G'`, `'C'`, `'T'`, or `'A'` it will crash.

Partial functions display behavior (e.g. crashing) that is not documented in their types.
This tends to make reasoning about code that uses them more difficult.
For this reason, partial functions are generally to be avoided.

Partiality is less objectionable in local functions than in global ones, because in local contexts it is easier to make sure that functions are never applied to problematic arguments.
Indeed, in the solution highlighted above it is clear that `transcribe` will never be applied to a problematic character, as if there were any such characters in `dna` then `find` would have returned `Just _` and not `Nothing`.

Still, it would be nice if it weren't necessary to check that `transcribe` is never applied to invalid characters.
`transcribe` is forced by its `Char -> Char` type to either be partial or else to return bogus values for some inputs &ndash; which would be similarly undesirable.
But another type, such as `Char -> Maybe Char`, would allow `transcribe` to be total.
The other approaches use such a variant.

This approach has the input walked twice (or thrice).
It is possible to solve this problem by walking the input only once.
The other approaches illustrate how.


[wiki-partial-functions]:
    https://wiki.haskell.org/Partial_functions
    "Haskell Wiki: Partial functions"
