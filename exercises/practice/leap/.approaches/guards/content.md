```haskell
isLeapYear :: Integer -> Bool
isLeapYear year
  | indivisibleBy 4   = False
  | indivisibleBy 100 = True
  | indivisibleBy 400 = False
  | otherwise         = True
  where
    indivisibleBy d = year `mod` d /= 0
```

# Guards

Guards can optionally be added to patterns to constrain when they should match.
For example, in

```haskell
ageCategory = case age of
  Just n | n >= 24 -> "Adult"
  Just n           -> "Nonadult"
  Nothing          -> "Eternal"
```

the pattern `Just n` will match both values `Just 5` and `Just 39`, but the pattern `Just n | n >= 24` will only match the latter.
Because patterns are checked in order, here

- `Just 39` will match the first pattern and so result in `"Adult"`, but
- `Just 5` will fall through to be matched to the second pattern, which will match, resulting in `"Nonadult"`.

Patterns may contain multiple guards in sequence.
These will then be checked in order just like patterns.
The following variant on the above example produces exactly the same result.

```haskell
ageCategory = case age of
  Just n | n >= 24   -> "Adult"
         | otherwise -> "Nonadult"
  Nothing            -> "Eternal"
```

Here there is one fewer pattern, but the first one contains one more guard.
`otherwise` is a synonym of `True`: it is the guard that always succeeds.

Sequences of guards are analogous to `if`&ndash;`else if` chains in other languages.


# In this approach

When there are not many cases to match against, it is common to use _function definition [syntactic sugar][wikipedia-syntactic-sugar]_ instead of `case` because sometimes that is a bit nicer to read.

```haskell
categorize (Just n)
  | n >= 24        = "Adult"
  | otherwise      = "Nonadult"
categorize Nothing = "Eternal"
-- is equivalent to / an abbreviation of
categorize age = case age of
  Just n | n >= 24   -> "Adult"
         | otherwise -> "Nonadult"
  Nothing            -> "Eternal"
```

In the case of Leap, there aren't any interesting patterns to match against, so we only match against a name.

```haskell
-- A "binding pattern", just like `n` above.
--          👇
isLeapYear year
  | ...
```

It turns out that, if we are careful to ask questions in the right order, we can always potentially attain certainty about the answer by asking one more question.

- If the year is not divisible by 4, then it is _certainly not_ a leap year.
- If it is, then it _might_ be a leap year.
  - If divisible by 4 but not by 100, then it _certainly is_ a leap year.
  - If also divisible by 100, then it _might_ be a leap year.
    - If divisible by 4 and 100 but not by 400, then it is _certainly not_ a leap year.
    - Otherwise, i.e. if also divisible by 400, then it _certainly is_ a leap year.

We can encode this sequence of checks using guards as follows.

```haskell
isLeapYear year
  | indivisibleBy 4   = False
  | indivisibleBy 100 = True
  | indivisibleBy 400 = False
  | otherwise         = True
  where
    indivisibleBy d = year `mod` d /= 0
```

We need not start checking for divisibility by 4 specifically.
Starting with 400 is also possible, but our checks and outcomes will be flipped:

```haskell
isLeapYear :: Integer -> Bool
isLeapYear year
  | divisibleBy 400 = True
  | divisibleBy 100 = False
  | divisibleBy   4 = True
  | otherwise       = False
  where
    divisibleBy d = year `mod` d == 0
```

Starting with 100 is more complicated: both years divisible by 100 and years not divisible by 100 sometimes are and sometimes aren't leap years.
Using guards is still possible, but it necessarily looks different:

```haskell
isLeapYear year
  | divisibleBy 100 = divisibleBy 400
  | otherwise       = divisibleBy   4
  where
    divisibleBy d = year `mod` d == 0
```

This is very similar to the [conditional expression approach][conditional-expression].



# When to use guards?

Many beginning Haskellers write code like

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe x m
  | isJust m  = fromJust m
  | otherwise = x
```

or

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe x m
  | m == Nothing = x
  | otherwise    = fromJust m
```

Don't do this.
Use `case` instead, whenever possible.
The compiler will be much more able to help you if you do, such as by checking that you have covered all possible cases.
It is also nicer to read.

Use guards

- to narrow down when patterns should match, or
- in lieu of other languages' `if`&ndash;`else if` chains.

Because guards are not themselves expressions, the latter use is not always possible.
In such cases, the [`MultiWayIf` language extension][multiwayif-extension] has your back:

```haskell
{- LANGUAGE MultiWayIf -}  -- at the top of the file

_ = if | condition   -> expression
       | proposition -> branch
       | otherwise   -> alternative
-- which is syntactic sugar for
_ = case () of
  _ | condition   -> expression
  _ | proposition -> branch
  _ | otherwise   -> alternative
```

For more on this question, see [Guards vs. if-then-else vs. cases in Haskell][so-guards-if-cases] on StackOverflow.


[conditional-expression]:
    https://exercism.org/tracks/haskell/exercises/leap/approaches/conditional-expression
    "Approach: a conditional expression"


[multiwayif-extension]:
    https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multiway_if.html
    "GHC Users Guide: Multi-way if-expressions"
[so-guards-if-cases]:
    https://stackoverflow.com/questions/9345589/
    "StackOverflow: Guards vs. if-then-else vs. cases in Haskell"
[wikipedia-syntactic-sugar]:
    https://en.wikipedia.org/wiki/Syntactic_sugar
    "Wikipedia: Syntactic sugar"
