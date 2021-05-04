# Hints

You need to implement the function

```haskell
value :: (Color, Color) -> Int
```

that only accepts two colors and produces the resistor's numeric value
component of its resistance.

**Bonus - Property Testing:**

This exercise is a good opportunity to write some [property tests](https://en.wikipedia.org/wiki/Property_testing). If you haven't written any before here are some resources:

1. [School of Haskell](https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing).
2. [Real world Haskell](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)

We have provided one sample QuickCheck property test in the test file.

Examples of properties that can be tested:

- `value (x, y) >= 10` for all `(x, y)` where `x` isn't `Black`

- Calling `show` on `value (x, y)` produces the `reverse` String of calling `show` on `value (y, x)`.

- All colors have unique names. (For any two colors, if the colors are different, so are their names.)
