# Recursion

```haskell
collatz :: Integer -> Maybe Integer
collatz n = case compare n 1 of
  LT -> Nothing
  EQ -> Just 0
  GT -> (1 +) <$> collatz (if even n then n `div` 2 else 3 * n + 1)
```

The number of steps it takes to get to `1` is one plus however many more steps it will take after taking one step.
This suggest a recursive solution.


## Recursion

[Recursion][wikipedia-recursion] in Haskell is the phenomenon of functions or values being defined in terms of themselves.
For example, here is a recursive definition of an (infinite) list:

```haskell
-- A recursive (constant) value
ones :: [Integer]
ones = 1 : ones
--          👆
--      recursion

-- `ones` is infinite, so take care to
-- ask for only finitely many elements

-- >>> take 5 ones
-- [1,1,1,1,1]
```

And here is a recursive definition of the well-known [factorial function][wikipedia-factorial]:

```haskell
-- A recursive function
factorial :: Integer -> Integer
factorial n
  | n <= 0    = 1
  | otherwise = n * factorial (n - 1)
--                      👆
--                  recursion

-- >>> factorial 5 == 5 * 4 * 3 * 2 * 1 * 1
-- True
```

This `factorial` function will always produce an output, because

- it contains a _base case_ that directly results in output, and
- it only ever recursively applies itself to 'smaller' values than its own argument, and
- any series of ever 'smaller' values is guaranteed to eventually hit one of the base cases.

In the case of `factorial 4` above we have

```
factorial 4
 = (4 * factorial 3)
 = (4 * (3 * factorial 2))
 = (4 * (3 * (2 * factorial 1)))
 = (4 * (3 * (2 * (1 * factorial 0))))
 = (4 * (3 * (2 * (1 * 1))))
 = (4 * (3 * (2 * 1)))
 = (4 * (3 * 2))
 = (4 * 6)
 = 24
```

Haskell does not provide any dedicated loop devices such as many other languages' `for` and `while` constructs.
Instead, all 'loopiness' in Haskell is produced through recursion &ndash; if not by you then under the hood of some of the functions you use.


## In this approach

First, we compare the input with `1`.
If it is less than `1` the input is invalid and we return `Nothing`.
If it is exactly `1`, then the number of steps it takes to reach `1` is zero &ndash; because we're already there &ndash; so we return `Just 0`.

If the input is greater than `1`, it'll take at least one step to get to `1`.
This step we calculate using the Collatz formula:

```haskell
nextStep n = if even n then n `div` 2 else 3 * n + 1
```

After this step it'll take some more (possibly zero) steps to reach `1`.
To compute how many, we recursively call `collatz`.
The total number it will take to get to `1` is one more than whatever the recursive call finds.

The recursive calls produces a `Maybe Integer` rather than just an `Integer`, so we cannot just increment it.
Nothing that a bit of pattern matching won't fix, however:

```haskell
theAnswer =
  case collatz (if even n then n `div` 2 else 3 * n + 1) of
    Nothing -> Nothing
    Just steps -> Just (steps + 1)
```

This pattern of mapping `Nothing` to `Nothing` and `Just` to `Just` is very common.
The function `fmap` was invented to do it for us:

```haskell
-- >>> fmap (+1) Nothing
-- Nothing

-- >>> fmap (+1) (Just 6)
-- Just 7
```

It makes the code a lot more compact.

```haskell
theAnswer =
  fmap (1 +) (collatz (if even n then n `div` 2 else 3 * n + 1))
```

The solution highlighted at the top is slightly different: it uses `<$>` instead of `fmap`.

`<$>` is a synonym of `fmap` and so does exactly the same thing: `fmap f xs == f <$> xs` always.
Sometimes using the operator results in more readable code, but it might take some getting used to.

```haskell
theAnswer =
  (1 +) <$> collatz (if even n then n `div` 2 else 3 * n + 1)
```

## Considerations on this approach

In a way, this solution is elegant.
However, it suffers an inefficiency.

For example, evaluation of `collatz 16` might go as follows.

```
collatz 16
  = (1 +) <$> collatz 8
  = (1 +) <$> ((1 +) <$> collatz 4)
  = (1 +) <$> ((1 +) <$> ((1 +) <$> collatz 2))
  = (1 +) <$> ((1 +) <$> ((1 +) <$> ((1 +) <$> collatz 1)))
  = (1 +) <$> ((1 +) <$> ((1 +) <$> ((1 +) <$> Just 0)))
  = (1 +) <$> ((1 +) <$> ((1 +) <$> Just 1))
  = (1 +) <$> ((1 +) <$> Just 2)
  = (1 +) <$> Just 3
  = Just 4
```

A tower of nested `<$>` applications forms!
For every step a `<$>` layer is added.
For numbers that take a long time to reach `1` this can take a lot of memory.

(As it happens, Collatz stopping times [tend to be very short][wikipedia-collatz-conjecture-empirical-data].
But the general points still stands.)

It is impossible to collapse the tower early.
`<$>` needs to know whether its right operand is a `Nothing` or a `Just`, but this does not become clear until the last step has been taken.

For an example of how to avoid this problem, see the [worker&ndash;wrapper][worker-wrapper] approach.


[worker-wrapper]:
    https://exercism.org/tracks/haskell/exercises/collatz-conjecture/approaches/worker-wrapper
    "Approach: use a worker&ndash;wrapper construct"


[wikipedia-collatz-conjecture-empirical-data]:
    https://en.wikipedia.org/wiki/Collatz_conjecture#Empirical_data
    "Wikipedia: Collatz conjecture &ndash; Empirical data"
[wikipedia-factorial]:
    https://en.wikipedia.org/wiki/Factorial
    "Wikipedia: Factorial"
[wikipedia-recursion]:
    https://en.wikipedia.org/wiki/Recursion
    "Wikipedia: Recursion"
