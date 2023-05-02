# Recursion

```haskell
distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x : xs) (y : ys) =
  (if x /= y then (1 +) else id) <$> distance xs ys
distance _ _ = Nothing
```

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

If both inputs are empty, then they are of equal length and they do not have any differing elements, so `Just 0` should be returned.

If one input is empty but the other isn't, then the inputs certainly are of unequal length, and `Nothing` should be returned.

If both inputs are nonempty, we recursively determine the hamming distance between their tails.
We also compare their heads.
If the heads are unequal we should add 1 to the distance between the tails; otherwise we should leave it as is.

The recursive call produces a `Maybe Int` rather than just an `Int`, so we cannot just increment it.
Nothing that a bit of pattern matching won't fix, however:

```haskell
distance (x : xs) (y : ys) =
  case distance xs ys of
    Nothing -> Nothing
    Just d -> Just (if x /= y then 1 + d else d)
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
distance (x : xs) (y : ys) =
  fmap (\d -> if x /= y then 1 + d else d) distance xs ys
```

The solution highlighted at the top is slightly different in two ways.

First, it uses the `<$>` operator.
Being a synonym of `fmap`, it does exactly the same thing: `fmap f xs == f <$> xs` always.
Sometimes using the operator results in more readable code, but it might take some getting used to.

```haskell
distance (x : xs) (y : ys) =
  (\d -> if x /= y then 1 + d else d) <$> distance xs ys
```

Second, it uses an `if` that produces one of two _functions_ instead of one function that produces one of two _integers_.

Because functions are ordinary values like any other in Haskell, we can have them be produced by `if` expressions just like other types of values:

```haskell
someFunction b = if b then (* 2) else (+ 3)

-- >>> someFunction True 4
-- 8

-- >>> someFunction False 4
-- 7
```

The highlighted solution chooses what to do to with the result of the recursive call based on the comparison of the heads.
If they are unequal, it applies an increment (`(1 +)`).
Otherwise, it applies the do-nothing function (`id`).

```haskell
distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x : xs) (y : ys) =
  (if x /= y then (1 +) else id) <$> distance xs ys
distance _ _ = Nothing
```


## Considerations on this approach

In a way, this solution is elegant.
However, it suffers an inefficiency.

Suppose the inputs are

```haskell
xs = [0, 0, 0, 0]
ys = [0, 1, 0, 1]
```

Then evaluation of `distance xs ys` might go as follows.

```haskell
distance xs ys
  = distance [0, 0, 0, 0] [0, 1, 0, 1]
  = id <$> ( distance [0, 0, 0] [1, 0, 1] )
  = id <$> ( (1 +) <$> ( distance [0, 0] [0, 1] ) )
  = id <$> ( (1 +) <$> ( id <$> ( distance [0] [1] ) ) )
  = id <$> ( (1 +) <$> ( id <$> ( (1 +) <$> distance [] [] ) ) )
  = id <$> ( (1 +) <$> ( id <$> ( (1 +) <$> Just 0 ) ) )
  = id <$> ( (1 +) <$> ( id <$> Just 1 ) )
  = id <$> ( (1 +) <$> Just 1  )
  = id <$> Just 2
  = Just 2
```

A tower of nested `<$>` applications forms!
For every pair of elements in the inputs a `<$>` layer is added.
For long lists this can take a lot of memory.

It is impossible to collapse the tower early.
`<$>` needs to know whether its right operand is a `Nothing` or a `Just`, but this does not become clear until the end of at least one of the lists has been reached.

For an example of how to avoid this problem, see the [worker&ndash;wrapper][worker-wrapper] approach.


[worker-wrapper]:
    https://exercism.org/tracks/haskell/exercises/hamming/approaches/worker-wrapper
    "Approach: use a worker&ndash;wrapper construct"


[wikipedia-factorial]:
    https://en.wikipedia.org/wiki/Factorial
    "Wikipedia: Factorial"
[wikipedia-recursion]:
    https://en.wikipedia.org/wiki/Recursion
    "Wikipedia: Recursion"
