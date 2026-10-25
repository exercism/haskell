# About

[Guards][guards] are used as a complement to [pattern matching][exercism-pattern-matching].
Which we will cover in a later concept.
Guards allows us to have different implementations of a function depending on the value of the input.
They allow for more complex checks.

A guard statement is defined by a pipe `|` followed by a boolean expression, ending with an equal sign `=` and the functions body.
There can be multiple guard statements for a single function.
The guard statements is evaluated from top to bottom, and the first one that evaluates to `True` will be executed.
A guard statement allows for a `otherwise` statement, which is a catch-all for any value that doesn't match the previous guard statements.

```haskell
isEven :: Int -> String
isEven n
    | even n = "n is even"
    | otherwise = "n is odd"
```

We can also deffine our function and use it inside the guard statement.

```haskell
isEven' :: Int -> Bool
isEven' n = even n

isEven :: Int -> String
isEven n
    | isEven' n = "n is even"
    | otherwise = "n is odd"
```
