# Haskell Built-in Numeric Types

## Numbers

Haskell's standard library, known as ***Prelude***, defines  most basic numeric types: fixed sized integers (Int), arbitrary precision integers (Integer), single precision floating (Float), and double precision floating (Double). Other numeric types such as rationals and complex numbers are defined in additional libraries.

`Integers` hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits (guaranteed at least -2^29 to 2^29).

`Float` corresponds to the set of real numbers, limited by the precision of the computer.
Operations defined on numbers usually work on one type or the other, but not mixing them.

There are functions to convert between the two, such as `toFloat` which converts `Int` to `Float` and `round`, `floor`, `ceiling` or `truncate` which convert a `Float` to an `Int` with different semantics.

## Arithmetic operators

Haskell has three "raise to the power" operators which work differently and take different argument types.

- `**` Takes two floating point numbers and uses logarithms to compute the power.

- `^^` Takes a floating point and raises it to a positive or negative integer power.

- `^` Takes any numerical type and raises it to a positive integer power.

Conversion from an integer type (Int or Integer) to anything else is done using "fromIntegral". The target type is inferred automatically. For example:

```haskell 
n :: Integer
n = 6
x :: Float
x = fromIntegral n      --> x = 6.0

m :: Int
m = 7
y :: Double
y = fromIntegral m      --> y = 7.0
```

Division of integers is a little complicated. If you use the ordinary "/" operator on integers then you will get an error message (although the expression "4/3" does work because Haskell helpfully promotes literal integers to floats where necessary). Instead, integer division is done using a collection of named operators.

Haskell has a neat trick with operators: you can take any function that takes two arguments and use it like an operator by enclosing the name in back-ticks. So the following two lines mean exactly the same:

```haskell
 d = 7 `div` 3
 d = div 7 3
```

With that in mind, here are the integer division operators:

- `quot`: Returns the quotient of the two numbers. This is the result of division which is then truncated towards zero.

- `rem`: Returns the remainder from the quotient.

- `div`: Similar to "quot", but is rounded down towards minus infinity.

- `mod`: Returns the modulus of the two numbers. This is similar to the remainder, but has different rules when "div" returns a negative number.

Just as you can convert a function with two arguments into an operator, you can convert an operator into a function with two arguments by putting it in parentheses. So the following two lines mean the same thing:

```haskell
(+) 3 4
3 + 4
```

---
### Credits: 
The above text is modified from __The Haskell 98 Report__ by Simon Peyton Jones, used with permission to copy, distribute and modify for any purpose. 