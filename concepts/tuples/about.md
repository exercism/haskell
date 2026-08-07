# About

[Tuples][tuple] are used commonly to group information, they differ from lists in that they are fixed-size and can hold different data types.
Tuples are created using curly braces, `()`, and are often used to return multiple values from a function.

```haskell
tuple = (1, "abc", False)
```

This can be useful when you for example want to store a coordinate in a 2D space, then you know that the tuple will always have two elements, the x and y coordinate.

```haskell
coordinate = (3, 4)
```

## Accessing elements

Quite often you work with short tuples, and you can access the elements using the `fst` and `snd` functions.

```haskell
x = fst coordinate
-- x = 3
y = snd coordinate
-- y = 4
```

[tuple]: https://hackage.haskell.org/package/base/docs/Data-Tuple.html
