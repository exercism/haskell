# About

Haskell has a type, `Num`, which is the base type for all numeric types.
There are four main sub-types inside the Number type: [`Int`][int], [`Integer`][integer], [`Double`][double]  and [`Float`][float].

There are two types of integers types in Haskell, which represents whole numbers.
`Int` is a fixed-size integer, while `Integer` is an arbitrary precision integer.
An `Integer` holds any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits (guaranteed at least -2^29 to 2^29 - 1).

Note that when writing negative numbers, you should use parentheses to avoid ambiguity.

```haskell
1

(-1)
```

`Float` corresponds to the set of real numbers, limited by the precision of the computer.
Operations defined on numbers usually work on one type or the other, but not both.
`Double` is a floating-point number with double precision.

```haskell
10.5
```

## Converting between integers and floats

Using the `toFloat` function, allows you to convert an `Int` or `Integer` to a `Float`.

```haskell
toFloat 1
-- -> 1.0
```

There is also `fromIntegral` which converts any number types to the inferred type number type needed.

```haskell
fromIntegral 1
-- -> 1.0
```

To convert a `Float` to an `Int`, you can use the [`round`][round], [`floor`][floor], or [`ceiling`][ceiling] functions.
The `round` function rounds to the nearest integer, `floor` rounds down, and `ceiling` rounds up.

```haskell
round 1.5
-- -> 2

floor 1.5
-- -> 1

ceiling 1.5
-- -> 2
```

## Arithmetic operators

You can use the [basic arithmetic operators][math] on the numeric types.
The operators are `+`, `-`, `*`, `/`, and `%`.
When using these operators, the types of the numbers must match.
The `fromIntegral` function will convert so that the types match.

### Addition & Subtraction & Multiplication

The `+` operator is used for addition, the `-` operator is used for subtraction, and the `*` operator is used for multiplication.

| Operator | Example        |
| -------- | -------------- |
| `+`      | `4 + 6 => 10`  |
| `-`      | `15 - 10 => 5` |
| `*`      | `2 * 3 => 6`   |

### Division

Division is used to divide numbers.
The `/` operator is used for division.
The result will always be a float.

```haskell
4.0 / 2.5
-- -> 1.6

4 / 2
-- -> 2.0
```

~~~~exercism/caution
In some programming languages, when dividing by zero, the result will be an error.

In Haskell, when dividing by zero, the result is `Infinity` or `-Infinity`.
The only exception is dividing zero by zero, resulting in `NaN` (Not a Number).

```haskell
1 / 0
-- -> Infinity

(-1) / 0
-- -> -Infinity

0 / 0
-- -> NaN
```
~~~~

## Integer division

Integer division is used to divide integral numbers and get the whole part of the result.
Haskell has two integer division functions, which differ in how they round when the result is negative:

- `div` rounds down, towards negative infinity.
- `quot` rounds towards zero.

```haskell
6 `div` 2
-- -> 3

5 `div` 2
-- -> 2

(-5) `div` 2
-- -> -3

5 `quot` 2
-- -> 2

(-5) `quot` 2
-- -> -2
```

~~~~exercism/caution
Dividing by zero when using integer division results in an Exception.
This is different from normal division.
~~~~

### Remainder

The remainder of an integer division can be found using `mod` or `rem`.
They differ in the sign of the result when one of the numbers is negative:

- `mod` goes together with `div`, and its result has the same sign as the divisor.
- `rem` goes together with `quot`, and its result has the same sign as the dividend.

```haskell
5 `mod` 3
-- -> 2

(-5) `mod` 3
-- -> 1

5 `mod` (-3)
-- -> -1

5 `rem` 3
-- -> 2

(-5) `rem` 3
-- -> -2

5 `rem` (-3)
-- -> 2
```

For any `x` and `y` (with `y` not zero), these laws hold:

```haskell
(x `div` y) * y + (x `mod` y) == x
(x `quot` y) * y + (x `rem` y) == x
```

~~~~exercism/note
When both numbers are positive, `div` and `quot` give the same result, as do `mod` and `rem`.
In that case `quot` and `rem` can be slightly more efficient, as `div` and `mod` need extra steps to handle negative numbers.
~~~~

~~~~exercism/caution
Dividing by zero when using `mod` or `rem` results in an Exception.
This is different from normal division.

```haskell
1 `mod` 0
-- Exception: divide by zero
```
~~~~

## Exponentiation

Haskell has three "raise to the power" operators which work differently and take different argument types.

- `**` Takes two **floating point numbers** and uses logarithms to compute the power.
- `^^` Takes a **floating point** and raises it to a positive or negative **integer** power.
- `^` Takes **any numerical type** and raises it to a **positive integer** power.

```haskell
3.0 ** 2.1
-- -> 10.04510856630514

2.5 ^^ 2
-- -> 6.25

2 ^ 3
-- -> 8
```

## Priority and parentheses

Haskell allows parentheses(`()`), which can be used to group expressions.
This is useful when you want to change the order of operations.

When using multiple arithmetic operators, the order of operations is the same as in mathematics, also known as [PEMDAS][pemdas].
It follows the order of parentheses(`()`), exponents(`**`), multiplication(`*`) and division(`/`), and addition(`+`) and subtraction(`-`).

```haskell
2 + 3 - 4 * 4
-- -> -11

(2 + 3 - 4) * 4
-- -> 4
```

[pemdas]: https://en.wikipedia.org/wiki/Order_of_operations
[floor]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:floor
[ceiling]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:ceiling
[round]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:round
