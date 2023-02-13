# `zipWith`

```haskell
distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ length $ filter id (zipWith (/=) xs ys)
```

The most straightforward way of solving this problem is to

- first check that the lengths are equal, and then
- iterate over both inputs together to count their differences.


## Higher-order functions

Higher-order functions are functions that take functions as arguments.
Examples well-known even outside of Haskell are `map` and `filter`.

- `map` applies a function to all elements of a list.
  It returns a list of all the results.
  ```haskell
  -- >>> map (* 2) [1 .. 5]
  -- [2,4,6,8,10]
  ```
- `filter` applies a predicate to all elements of a list.
  It returns a list with only those elements for which predicate returned `True`.
  ```haskell
  -- >>> filter odd [1 .. 5]
  -- [1,3,5]
  ```

Another example that is extremely common in Haskell is the function composition operator `(.)`, which 'chains' two functions into one.

```haskell
reverseSort :: [Int] -> [Int]
reverseSort = reverse . sort  -- first sort, then reverse

-- >>> reverseSort [3, 1, 4, 1, 5, 9]
-- [9,5,4,3,1,1]
```

Still other examples include

- `zipWith`, which combines two lists into one using a element-combining function.
  ```haskell
  -- >>> zipWith (+) [10, 20, 30] [1, 2, 3]
  -- [11,22,33]
  ```
- `uncurry`, which turns a function of two arguments into one that accepts a tuple.
  ```haskell
  tupleMinus = uncurry (-)

  -- >>> tupleMinus (23, 4)
  -- 19
  ```
- `fmap`, which does the same as `map` except it also works for some types that aren't lists.
  ```haskell
  -- >>> fmap (+ 1) [1 .. 5]
  -- [2,3,4,5,6]

  -- >>> fmap (+ 1) (Just 7)
  -- Just 8
  ```


## In this approach

After making sure that the lengths are equal, we count the number of places in which the inputs differ.

The `/=` operator compares two values and returns `True` precisely when they are unequal.

```haskell
-- >>> 4 /= 5
-- True

-- >>> 2 /= 2
-- False
```

We use `zipWith` to walk both input lists simultaneously, marking unequal pairs using `(/=)` as we go.

```haskell
comparisons =
  zipWith (/=)
    [3, 2, 6]
    [5, 2, 4, 7]

-- >>> comparisons
-- [True,False,True]
```

`zipWith` stops as soon as one of its argument lists stops.
This is the reason we need to check the lengths separately.

Now, the number of differences between the two inputs is exactly the number of `True`s in the list produced by `zipWith`.
We count them using `filter` and `length`.

We need to give `filter` a predicate to filter by.
In this case, this predicate should return `True` when given `True` and `False` for `False`.
A function that does this already exists in the `Prelude`: `id` is the function that returns its argument unchanged.

We could also have zipped the two lists using the simpler `zip` function.

```haskell
pairs =
  zip
    [3, 2, 6]
    [5, 2, 4, 7]

-- >>> pairs
-- [(3,5),(2,2),(6,4)]
```

In that case we would have needed to count the pairs that have the same value in both places.
This can still be done using `filter`, but the required predicate is more complicated.
`uncurry (/=)` is a function that takes a tuple and returns `True` when the tuple has equal values in both places.

```haskell
distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ length $ filter (uncurry (/=)) (zip xs ys)
```


## Considerations on this approach

This style of solution is very easy to understand.
This is a very important quality for code to have!
Code is primarily meant for humans to reason about, after all.

On the other hand, this solution suffers an inefficiency.
Haskell's lists are linked lists.
Therefore, `length` needs to walk its argument entirely.
This can take a lot of time for long lists.

`length` is called three times in this approach, resulting in three separate walks over the inputs.
The [explicit recursion][recursion] and [worker&ndash;wrapper][worker-wrapper] approaches avoid this and instead walk the input exactly once.


[recursion]:
    https://exercism.org/tracks/haskell/exercises/hamming/approaches/recursion
    "Approach: recurse by hand"
[worker-wrapper]:
    https://exercism.org/tracks/haskell/exercises/hamming/approaches/worker-wrapper
    "Approach: use a worker&ndash;wrapper construct"
