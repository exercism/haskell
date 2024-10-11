# Numbers

Haskell has a type, `Num`, which is the base type for all numeric types.
There are four main sub-types inside the Number type: [`Int`][int], [`Integer`][integer], [`Double`][double]  and [`Float`][float].

There are two types of integers types in Haskell, which represents whole numbers.
`Int` is a fixed-size integer, while `Integer` is an arbitrary precision integer.
`Integer`s hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits (guaranteed at least -2^29 to 2^29).

Note that when writting negative numbers, you should use parentheses to avoid ambiguity.

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

To convert a `Float` to an `Int`, you can use the `round`, `floor`, or `ceiling` functions.
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

Integer division is used to divide numbers and get the whole part of the result.
The result will always be rounded down to an Int.

```haskell
2 `div` 2
-- -> 2

5 `div` 2
-- -> 2
```

~~~~exercism/caution
Dividing by zero when using integer division results in a Exception.
This is different from normal division.
~~~~

### Modulus

Modulus is used to get the remainder of a division.
The `mod` operator is used for modulus.

```haskell
5 `mod` 2
-- -> 1

5 `mod` 3
-- -> 2
```

~~~~exercism/caution
Dividing by zero when using modulo results in an Exception.
This is different from normal division.

```haskell
1 `mod` 0
# Exception: divide by zero
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
[math]: https://crystal-lang.org/reference/latest/tutorials/basics/30_math.html
[int]: https://crystal-lang.org/reference/latest/syntax_and_semantics/literals/integers.html
[float]: https://crystal-lang.org/reference/latest/syntax_and_semantics/literals/floats.html
