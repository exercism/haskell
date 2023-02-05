# Introduction

To solve this problem, you need to

- check that the two inputs have the same length, and
- if they do count the number of places in which they differ.

The desired output is `Nothing` if the inputs are not equally long, and `Just numberOfDifferences` otherwise.


## Approach: `zipWith`

```haskell
distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ length $ filter id (zipWith (/=) xs ys)
```

The most straightforward way of solving this problem is to

- first check that the lengths are equal, and then
- iterate over both inputs together to count their differences.

You can use `length` to find the lengths.
The differences between the two inputs you can find using `zipWith` and `filter`, both of which are examples of **higher-order functions**.

While this style of solution might suffer some inefficiency, it should not be discounted because it is so simple.

[Read more about this approach][zipwith]


## Approach: hand-written recursion

```haskell
distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x : xs) (y : ys) =
  (if x /= y then (1 +) else id) <$> distance xs ys
distance _ _ = Nothing
```

The standard library does not include a length-checking `zipWith`.
Therefore, if you do not want to use `length`, you'll have to write the recursion yourself.

The return type of the recursive call is `Maybe Int`.
This complicates incrementing the number of found differences.
You can use `fmap`/`<$>` for this.

[Read more about this approach][recursion]


## Approach: a worker&wrapper construct

```haskell
distance :: String -> String -> Maybe Int
distance = go 0
  where
    go !n (x : xs) (y : ys) = go (if x == y then n else n + 1) xs ys
    go !n [] [] = Just n
    go _ _ _ = Nothing
```

The above recursive solution is space-inefficient.
To avoid its buildup of computations-to-be-completed you can use a worker&ndash;wrapper construct.
This solution operates in constant space, but is considerably more complex than the naive solution that uses `length`.

[Read more about this approach][worker-wrapper]


[recursion]:
    https://exercism.org/tracks/python/exercises/hamming/approaches/recursion
    "Approach: recurse by hand"
[worker-wrapper]:
    https://exercism.org/tracks/python/exercises/hamming/approaches/worker-wrapper
    "Approach: use a worker&ndash;wrapper construct"
[zipwith]:
    https://exercism.org/tracks/python/exercises/hamming/approaches/zipwith
    "Approach: using zipWith"
