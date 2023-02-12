# Generate a list of steps

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
- counting the number of steps.

using only functions from the standard library for all of these except one.


## `iterate` and `unfoldr`

When you have a 'seed' element and a way of computing every next element from it, you can use `iterate` or `unfoldr` to produce the entire sequence.

`iterate` ([documentation][iterate]) uses the given function to iteratively compute the next element from the last one:

```haskell
powersOfTwo = iterate (2 *) 1

-- >>> take 5 powersOfTwo
-- [1,2,4,8,16]
```

The list produced by `iterate` is always infinite, so you might need to truncate it yourself.

`unfoldr` ([documentation][unfoldr]) is more general than `iterate`:

- In addition to determining what the next element should be, the given function also gets to decide when the list should end.
  It does this by returning a `Just` when the list should be extended with another element, and `Nothing` when the list should end.
- Also, new list elements are not computed from old ones, but instead from 'seed' values that can be of a different types.
  Every time a next element is computed, a new 'seed' for the rest of the list is produced as well.

In the following example we compute the letters of the alphabet from their place in the sequence.

```haskell
alphabet :: [Char]
alphabet = unfoldr nextLetter 0
  where
    nextLetter :: Int -> Maybe (Char, Int)
    nextLetter index
      | index == 26 = Nothing
      | otherwise = Just (chr (ord 'a' + index), index + 1)

-- >>> alphabet
-- "abcdefghijklmnopqrstuvwxyz"
```

`iterate` can be regarded as a special case of `unfoldr`.
Indeed, it could have been defined as

```haskell
iterate :: (a -> a) -> a -> [a]
iterate f = unfoldr (\x -> Just (x, f x))
```

## In this approach

Given any number, `nextStep` will compute the next Collatz number.
This is the only logic that is provided by us instead of by the standard library.

`nextStep` is used by `iterate` or `unfoldr` to generate all the steps.

The sequence of steps should be terminated at the first `1`.
`foldr` does this by itself, but `iterate` needs some help from `takeWhile`.

With a complete sequence of steps, the only thing that remains to be done is count them.
We simply use `length` for this.

Haskell's laziness helps this approach be efficient.
`length` requests elements from the list of steps one by one.
These elements are generated on demand and discarded shortly after.
The full list of elements is never realized in memory: here the list functions less like a container and more like a generator from some other languages.
As a result, this solution operates in constant memory.


[iterate]:
    https://hackage.haskell.org/package/base/docs/Prelude.html#v:iterate 
    "Documentation of iterate"
[unfoldr]: 
    https://hackage.haskell.org/package/base/docs/Data-List.html#v:unfoldr 
    "Documentation of unfoldr"
