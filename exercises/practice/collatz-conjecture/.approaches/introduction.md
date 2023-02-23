# Introduction

This problem requires iteratively computing the next number in a sequence, until `1` is reached.


## Approach: generate a list of steps

```haskell
-- Using `iterate`
collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just (countSteps n)
  | otherwise = Nothing
  where
    countSteps = toInteger . length . takeWhile (/= 1) . iterate nextStep
    nextStep k = if even k then k `div` 2 else 3 * k + 1

-- Using `unfold`
collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just (countSteps n)
  | otherwise = Nothing
  where
    countSteps = toInteger . length . unfoldr nextStep

    nextStep 1 = Nothing
    nextStep k = Just (k, if even k then k `div` 2 else 3 * k + 1)
```

This approach neatly disentangles all four concerns of

- computing the next Collatz step,
- computing the sequence of all Collatz steps,
- truncating this sequence at the first `1`, and
- counting the number of steps,

using only functions from the standard library for all of these except one.

[Read more about this approach][list-of-steps].


## Approach: recursion

```haskell
collatz :: Integer -> Maybe Integer
collatz n = case compare n 1 of
  LT -> Nothing
  EQ -> Just 0
  GT -> (1 +) <$> collatz (if even n then n `div` 2 else 3 * n + 1)
```

While arguably elegant, this approach suffers space usage linear in the number of steps.

[Read more about this approach][recursion].


## Approach: a worker&ndash;wrapper construct

```haskell
collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just (go 0 n)
  | otherwise = Nothing
  where
    go !acc 1 = acc
    go !acc k = go (acc + 1) (if even k then k `div` 2 else 3 * k + 1)
```

To avoid the excessive space usage of the above recursive approach, we can employ a worker&ndash;wrapper construct.
This approach is as efficient as the [generating a list of steps approach][list-of-steps], but significantly harder to understand.

[Read more about this approach][worker-wrapper].


[list-of-steps]:
    https://exercism.org/tracks/haskell/exercises/collatz-conjecture/approaches/list-of-steps
    "Approach: generate a list of steps"
[recursion]:
    https://exercism.org/tracks/haskell/exercises/collatz-conjecture/approaches/recursion
    "Approach: recurse by hand"
[worker-wrapper]:
    https://exercism.org/tracks/haskell/exercises/collatz-conjecture/approaches/worker-wrapper
    "Approach: use a worker&ndash;wrapper construct"
