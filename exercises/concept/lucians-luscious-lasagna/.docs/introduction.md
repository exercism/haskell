# Introduction

## Constants and functions

A constant value is defined with `name = expression`.
In Haskell, everything except definitions are expressions.

```haskell 
five = 5

six = 3 + 3
```

Functions are defined with `name parameters... = expression`, parameters being only separated by space.

```haskell
add number1 number2 = number1 + number2
```

Invoking a function also is an expression and is done by specifying its name and passing arguments for each of the function's parameters,
separated by space, just like for function definition.

```haskell
five = add 2 3
```

Parentheses can be used to specify the order of evaluation of an expression.

```haskell
six = add 2 (1 * 4)

twelve = add 2 1 * 4
```

## Indentation / significant whitespace

Haskell uses whitespaces and indentation to specify code boundaries. 
***Code which is part of some expression should be indented further in than the beginning of that expression.***

- Module, import, and top-level function definitions must start at the left most column.
- If an expression is split into multiple lines, the code that is part of that expression must be indented under that expression with at least one space.
- Parts of the expression that are grouped together should be indented with equal number of spaces.

```haskell
-- A function split over multiple lines, so subsequent lines must be indented
add number1 number2 =
    number1 + number2
```

## Modules

A module exports functions. That means when I import a module, I can use the functions that it exports. 
The module can define functions that it uses internally, but we can only see and use the ones that it exports.

At the beginning of a module, we specify the module name. If we have a file called Calculator.hs, then we should name our module Calculator. 
Then, we specify the functions that it exports and after that, we can start writing the functions. So we'll start with this.


```haskell
module Calculator ( add ) where
```

## Comments

A comment is some text within the Haskell file that is not interpreted as code.
It is mainly intended to be read by yourself and other programmers.
There is a lightweight syntax for single line comments, based on double dashes.
Multiline comments are also possible with the `{-` and `-}` pair
of opening and closing delimiters.

```haskell
-- a single line comment
-- another single line comment

{- a multiline comment
   spanning multiple lines
-}
```

