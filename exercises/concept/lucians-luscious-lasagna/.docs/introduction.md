# Basic

Haskell is a functional programming language, there functions are treated as first-class citizens.
This means that functions can be passed as arguments to other functions, returned as values from other functions, and assigned to variables.

## Functions

Functions in Haskell are defined using the `=` operator.
The name of the function is followed by its arguments, separated by spaces.
This is quite different from most other languages where the function name is followed by parentheses containing the arguments separated by commas.

```haskell
add number1 number2 = number1 + number2
```

Haskell is a statically typed language, the ghc compiler can infer the types of the arguments and the return value.
However, it is a good practice to specify the types of the arguments and the return value.
To specify the type of a function, we use the `::` operator.
The name of the function is followed by the `::` operator, then the type of the arguments separated by `->`, the last type is the return value.

```haskell
justNumber :: Int
justNumber = 42

add :: Int -> Int -> Int
add number1 number2 = number1 + number2
```

## Invoking functions

To invoke a function, we write the function name followed by the arguments separated by spaces.

```haskell
add 1 2
```

When passing a function as an argument to another function, we can have to use parentheses to specify the order of evaluation.

```haskell
add (add 1 2) 3
```

There 1 2 is evaluated first, then the result is passed to the outer add function, so the outer add function recives two arguments.

## Indentation / significant whitespace

Haskell uses whitespaces and indentation to specify code boundaries.
***Code which is part of some expression should be indented further in than the beginning of that expression.***

- Module, import, and top-level function definitions must start at the leftmost column.
- If an expression is split into multiple lines, the code that is part of that expression must be indented under that expression with at least one space.
- Parts of the expression that are grouped together should be indented with equal numbers of spaces.

```haskell
-- A function split over multiple lines, so subsequent lines must be indented
add number1 number2 =
    number1 + number2
```

https://en.wikibooks.org/wiki/Haskell/Indentation

## Modules

Modules export functions.
That means you can use the functions the module makes available in its export statement.
A module can also define functions only for internal use.
By default, functions that are not explicitly exported cannot be used from outside the module.

At the beginning of a module, we specify the module name.
If we have a file called Calculator.hs, then we should name our module Calculator.
Then, we specify the functions that it exports and after that, we can start writing the functions.
So we'll start with this.

```haskell
module Calculator ( add ) where
```

https://learnyouahaskell.github.io/modules#making-our-own-modules

## Comments

A comment is some text within the Haskell file that is not interpreted as code.
It is mainly intended to be read by yourself and other programmers.
There is a lightweight syntax for single line comments, based on double dashes.
Multiline comments are also possible with the `{-` and `-}` pair of opening and closing delimiters.

```haskell
-- a single line comment
-- another single line comment

{- a multiline comment
   spanning multiple lines
-}
```

## Formatting

There is a [style guide](https://kowainik.github.io/posts/2019-02-06-style-guide), and [Stylish-Haskell](https://github.com/haskell/stylish-haskell) can be used to automatically format code.

### Credits

This concept guide borrows liberally from [Learn You a Haskell for Great Good!](https://learnyouahaskell.github.io/chapters),
under [Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License](https://creativecommons.org/licenses/by-nc-sa/3.0/)
