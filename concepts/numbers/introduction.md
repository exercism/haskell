# Introduction

`Integer`s hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits (guaranteed at least -2^29 to 2^29).

`Float` corresponds to the set of real numbers, limited by the precision of the computer.
Operations defined on numbers usually work on one type or the other, but not both.

Functions to convert between `Float` and `Integer` include, among others, `toFloat` which converts `Int`/`Integer` to `Float` and `ceiling` which converts a `Float` to an `Int`.

Conversion from an integer type (`Int` or `Integer`) to any other numeric type is done using the function `fromIntegral`.
The target type is inferred automatically.
For example:

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
