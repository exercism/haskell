# Hints

To complete this exercise you need to implement the function `score`,
that takes a sequence of bowling *rolls* and returns the final score or
the appropriate error:

```haskell
score :: [Int] -> Either BowlingError Int

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
                  deriving (Eq, Show)
```

You will find these definitions already in place, but it is up to you
to define the function.

Keep in mind that the test suite expects the rolls to be numbered
starting from zero.
