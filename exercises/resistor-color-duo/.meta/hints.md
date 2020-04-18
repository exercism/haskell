## Hints

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

We have provided one sample property test in the [test file](test/Tests.hs). We use the [QuickCheck](https://hackage.haskell.org/package/QuickCheck) library, but there are [others](http://hackage.haskell.org/package/hedgehog) as well, feel free to play around.

Few properties that can be tested:

* `value (x, y) >= 10` for all x except Black and all y

* `show (value (x, y)) == reverse (show (value (y, x)))` for all x, y except Black.

* (Color, Color) is isomorphic to [0..99] (or: all colors have a unique value).

* Associativity: Kind of tricky because you can't get the value of a single Color:

```haskell
value (k, x) <= value (k, y) && value (k, y) <= value (k, z)
  ⇒ value (k, x) <= value (k, z)

value (x, k) <= value (y, k) && value (y, k) <= value (z, k)
  ⇒ value (k, x) <= value (k, z)
```

Can you think of any other properties?
