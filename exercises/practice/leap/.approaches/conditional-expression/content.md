# Conditional expression

```haskell
isLeapYear :: Integer -> Bool
isLeapYear year =
  if divisibleBy 100
    then divisibleBy 400
    else divisibleBy 4
  where
    divisibleBy d = year `mod` d == 0
```


## Conditional expressions

A _conditional expression_ (`if … then … else …`) is a compound expression that uses a test to determine which of two alternatives to evaluate to.
Many other languages feature a similar construct, often termed 'ternary operator'.
They are also known as _`if` expressions_.

When `p` is some expression of type `Bool` and `t` and `f` are any two expressions of the same type, then `if p then t else f` will

- evaluate to `t` if `p` evaluates to `True`, and
- evaluate to `f` if `p` evaluates to `False`.

~~~~exercism/note
Conditional expressions are [syntactic sugar][wikipedia-syntactic-sugar] for certain `case` expressions:

```haskell
_ = if p then t else f
-- is an abbreviation of
_ = case p of
  True  -> t
  False -> f
```
~~~~


## In this approach

This approach uses exactly two tests to determine whether a year is a leap year.

The first test is for divisibility by 100.
Once we know if the year is a multiple of 100, we know which further test to perform.

- If the year is evenly divisible by 100, then `divisibleBy 100` will evaluate to `True` and the entire `if` expression will evaluate to whatever `divisibleBy 400` evaluates to.
- If the year is _not_ evenly divisible by 100, then `divisibleBy 100` is `False` and so the `if` expression evaluates to `divisibleBy 4`.


## When to use `if`?

`if` expressions might be a good fit when you

- need an expression that
- chooses between exactly two options
- depending on a single `Boolean`.

When you have something other than a `Boolean`, use `case` instead.

When you do not strictly need an expression, an alternative is to [use guards][guards].

When you need to choose between more than two options, guards might be the solution.
However, guards are not expressions and so are not always applicable.
In such cases you might want to break out a multi-way `if`, available through the [`MultiWayIf` language extension][multiwayif-extension]:

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


## An example of lazy evaluation

Just like 'ternary operators' in other languages, conditional expressions evaluate lazily.
Specifically, only the relevant branch is evaluated:

```haskell
ghci> error "Crash!"  -- for demonstration
*** Exception: Crash!
ghci> if even 42 then "Success!" else error "Crash!"
"Success!"
```

Notice how evaluating the entire `if` expression does not result in a crash, even though one of its branches would if it were evaluated.

In our solution above we have

| year | `divisibleBy 100` | `divisibleBy 400` | `divisibleBy 4` | is leap year |
| ---- | ----------------- | ----------------- | --------------- | ------------ |
| 2020 | `False`           | not evaluated     | `True`          | `True`       |
| 2019 | `False`           | not evaluated     | `False`         | `False`      |
| 2000 | `True`            | `True`            | not evaluated   | `True`       |
| 1900 | `True`            | `False`           | not evaluated   | `False`      |


[guards]:
    https://exercism.org/tracks/haskell/exercises/leap/approaches/guards
    "Approach: a sequence of guards"


[multiwayif-extension]:
    https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/multiway_if.html
    "GHC Users Guide: Multi-way if-expressions"
[so-guards-if-cases]:
    https://stackoverflow.com/questions/9345589/
    "StackOverflow: Guards vs. if-then-else vs. cases in Haskell"
[wikipedia-syntactic-sugar]:
    https://en.wikipedia.org/wiki/Syntactic_sugar
    "Wikipedia: Syntactic sugar"
