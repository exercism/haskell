# Worker&ndash;wrapper

```haskell
distance :: String -> String -> Maybe Int
distance = go 0
  where
    go !n (x : xs) (y : ys) = go (if x == y then n else n + 1) xs ys
    go !n [] [] = Just n
    go _ _ _ = Nothing
```

This approach uses an inner _worker_ function to simultaneously walk both lists and keep track of the number of encountered differences.
It also uses a _bang pattern_ to force intermediate evaluation, to guarantee decent space efficiency.


## The worker&ndash;wrapper construct

Sometimes, when solving a problem, it is more convenient or efficient to keep track of some kind of _state_.
Many other languages use local variables for this.
However, variables do not exist in Haskell: all bindings are constants.

In lieu of local variables we can add extra parameters to our functions.
These will hold our state.
'Changing' the state is done by calling the same functions (recursively) with different arguments.

Functions that uses such additional parameters to represent state are often called _workers_, and functions that use workers to solve a problem in a stateful way are often called _wrappers_.

As an example, consider the following possible definition of `length`.

```haskell
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
```

One problem with this function is that it builds large computations that take up potentially a lot of memory:

```haskell
_ =  length "abcde"
  == 1 + length "bcde"
  == 1 + (1 + length "cde")
  == 1 + (1 + (1 + length "de"))
  == 1 + (1 + (1 + (1 + length "e")))
  == 1 + (1 + (1 + (1 + (1 + length ""))))
  -- this computation is as large as the list was long!
  == 1 + (1 + (1 + (1 + (1 + 0))))
  == 1 + (1 + (1 + (1 + 1)))
  == 1 + (1 + (1 + 2))
  == 1 + (1 + 3)
  == 1 + 4
  == 5
```

It would be more efficient to keep track of a running total number of encountered elements.
To do this we can use a variant of `length` with an additional parameter:

```haskell
length' :: Int -> [a] -> Int
length' count []     = count
length' count (_:xs) = length' (1 + count) xs
```

Here the parameter `count` is used to keep track of the number of previously encountered elements.
It might evaluate as follows.

```haskell
_ =  length' 0 "abcde"
  == length' 1 "bcde"
  == length' 2 "cde"
  == length' 3 "de"
  == length' 4 "e"
  == length' 5 ""
  == 5
```

The computation stays small at every step &ndash; much more efficient!

To obtain an efficient function `length` with the familiar `[a] -> Int` type &ndash; instead of the more involved `Int -> [a] -> Int` &ndash; we can 'wrap' `length'` in a function that provides it with initial arguments:

```haskell
length :: [a] -> Int
length xs = length' 0 xs
  where
    length' count []     = count
    length' count (_:xs) = length' (1 + count) xs
```

Here `length` is known as the _wrapper_, and `length'` as the _worker_.
Hence, this is an example of the _worker&ndash;wrapper_ construct.

The worker is commonly called `go`, `loop`, or `f'` when the wrapper is called `f`.

The worker&ndash;wrapper pattern is more general than demonstrated here.
For a few more more examples, see the [relevant wiki article][wiki-worker-wrapper], and for yet more examples and an explanation of the general pattern see the paper «[The worker/wrapper transformation][paper-worker-wrapper]» (pdf).


## Bang patterns

The above implementation of `length'` _might_ evaluate as efficiently as illustrated.
However, depending on which optimization opportunities are spotted by the compiler, it also might not!
The definition of `length'` allows evaluation to proceed as follows as well.

```haskell
_ =  length' 0 "abcde"
  == length' (1 + 0) "bcde"
  == length' (1 + (1 + 0)) "cde"
  == length' (1 + (1 + (1 + 0))) "de"
  == length' (1 + (1 + (1 + (1 + 0)))) "e"
  == length' (1 + (1 + (1 + (1 + (1 + 0))))) ""
  == 1 + (1 + (1 + (1 + (1 + 0))))
  == 1 + (1 + (1 + (1 + 1)))
  == 1 + (1 + (1 + 2))
  == 1 + (1 + 3)
  == 1 + 4
  == 5
```

Still a computation as large as the list was long is built.
The situation is not quite as bad as before though.

In the case of the naive implementation of `length`, the built up expression has the form

```haskell
_ = 1 + (1 + (⋯(1 + length …)⋯))
```

This expression cannot be simplified before `length …` has been evaluated.
That is, simplification cannot begin until the last recursive step has been evaluated.
As a consequence, the computation must grow as big as the original list.

The computation built up by `length'` on the other hand has the form

```haskell
_ = 1 + (1 + (⋯(1 + 0)⋯))
```

This _can_ be simplified!
In fact, as illustrated in the previous section, it can be simplified at every individual step.

Haskell offers various ways to force intermediate evaluation.
One convenient tool is the _bang pattern_, enabled by the `BangPatterns` language extension.

```haskell
{-# LANGUAGE BangPatterns #-}  -- 👈 at the top of the file

length'' :: Int -> [a] -> Int
length'' !count []     = count
length'' !count (_:xs) = length'' (1 + count) xs
--       👆
--  bang patterns
```

Bang patterns force evaluation of arguments at function invocation.
Consequently, this `length''` (at worst) evaluates as follows.

```haskell
_ =  length'' 0 "abcde"
  == length'' (1 + 0) "bcde"
  == length'' (1 + 1) "cde"
  == length'' (1 + 2) "de"
  == length'' (1 + 3) "e"
  == length'' (1 + 4) ""
  == 1 + 4
  == 5
```

Here, at each step the `(1 + …)` argument is reduced to a single number first, before again `1` is added to it.

We can eliminate even that last recurring `+` using another strictness management tool: the `seq` function.

```haskell
length''' :: Int -> [a] -> Int
length''' count []     = count
length''' count (_:xs) =
  let count' = 1 + count
   in count' `seq` length''' count' xs
```

``x `seq` y`` always returns `y`, but additionally guarantees that `x` is evaluated if `y` is evaluated.
Here, `length'''` uses it to guarantee that `count'` is evaluated 'before' it is passed to the recursive `length'''` call.
As a result, `length'''` evaluates optimally.

A full discussion of strictness in Haskell here would take up too much space.
Please consult your other learning resources.
The track docs include an article on [Haskell learning resources][learning-resources].


[learning-resources]:
    https://exercism.org/docs/tracks/haskell/learning
    "How to learn Haskell"


[wiki-worker-wrapper]:
    https://wiki.haskell.org/Worker_wrapper
    "Haskell Wiki: Worker wrapper"
[paper-worker-wrapper]:
    http://www.cs.nott.ac.uk/~pszgmh/wrapper.pdf
    "(pdf) The worker/wrapper transformation"
