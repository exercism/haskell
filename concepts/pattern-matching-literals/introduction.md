# Introduction

While `if/else` expressions can be used to execute conditional logic, Haskell also has a more powerful way to execute conditional logic: [pattern matching][PM].
With pattern matching, a value can be tested against one or more _patterns_.
An example of such a pattern is the _constant pattern_, which matches a value against a constant (e.g. `1` or `"hello"`).

When defining functions, you can define separate function bodies for different patterns.
This leads to clean code that is simple and readable.
You can pattern match on any data type — numbers, characters, lists, tuples, etc.

For example, a trivial function that takes a whole number (`Int`) and makes it _1_ closer to _0_ could be expressed like this:

```haskell
closerToZero :: Int -> Int
closerToZero 0 = 0
closerToZero 1 = 0
```

Pattern matching starts to shine when used together with other patterns, for example the _variable pattern_:

```haskell
closerToZero :: Int -> Int
closerToZero 0 = 0
closerToZero n = n - 1
```

The above example treats all inputs other than _0_ the same, and would produce incorrect results for negative numbers.
This can be solved using conditional patterns, known as _guards_, which are expressed with the `|` symbol:

```haskell
closerToZero :: Int -> Int
closerToZero n
    | n < 0 = n + 1
    | n > 0 = n - 1
```

In the above examples not all possible inputs have a matching pattern.
The compiler will detect this and output a warning.
This is a very useful feature of Haskell that helps ensure all possible paths are covered to avoid run-time errors.
It is known as _exhaustive checking_.
To solve the warning, you have to handle all cases.
Within _guards_ you can use the expression `otherwise` as syntactic sugar for `True` to catch all remaining patterns.

```haskell
closerToZero :: Int -> Int
closerToZero n
    | n < 0 = n + 1
    | n > 0 = n - 1
    | otherwise = 0
```

Pattern matching will test a value against each pattern from top to bottom, until it finds a matching pattern and executes the logic associated with that pattern.
**The order of patterns matters!**

[PM]: https://learnyouahaskell.github.io/syntax-in-functions#pattern-matching
