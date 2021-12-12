# Haskell Syntax Basics 2

## Exporting functions

Exporting functions was covered in the first concept, here is a quick refresher.

Each file in Haskell is a module and must contain a `module` statement before all other code.

Module names must match their file name, so module `Calculator` must be in file Calculator.hs, and module `Parser.Utils` needs to be in file Parser/Utils.hs.

Anything defined within a module is privatly scoped to it and cannot be accessed from outside the module unless listed in the module definition.

```haskell
-- Calculator.hs
-- Defines the Calculator module, and exposes everything within by default e.g. `six` and `add`.
module Calculator where

add number1 number2 = number1 + number2

six = add 3 3
```

```haskell
-- Calculator.hs
-- Defines the Calculator module, and exposes the `add` function only.
module Calculator ( add ) where

add number1 number2 = number1 + number2

six = add 3 3
```

## Importing functions from other modules

Accessing functions defined in other modules is done via imports.

With open imports, all functions within that module that are exposed by it are made accessible by default.
Open imports enable direct access to the exposed functions within that module, without prefixing.  If you do not specify which functions you want exposed, all exported functions become available. If you just need a couple of functions from a module, you can selectively import just those functions (for example `map`).

```haskell
-- open imports
import Data.List                   -- length, nub, ...
import Data.List ( length )        -- length
```

Imports can be qualified to avoid name-space conflicts, and accessing a function (e.g. the `length` and `nub` functions in the `List` module) is done by prefixing the module name (`Data.List.length`).

Qualified imports are preferred to aid clarity and avoid name clashes.

```haskell
-- qualified imports
import qualified Data.List            -- Data.List.length, Data.List.nub, ...
import qualified Data.List ( length ) -- Data.List.length 
```

Finally, typing out `Data.List.Map` in front of every function from that module gets tedious. To help with this we can rename the qualified import to something shorter:

```haskell
import qualified Data.List as DL  
```

Now, to reference Data.List's `length` function, we just use `DL.length`.


You can also import types, and their constructors from other modules, but that is for a later concept.

## Type annotations

Type annotations are defined with `name : parameter types -> return type`, parameter types also being separated by `->`.

```haskell
add : Int -> Int -> Int
add number1 number2 = number1 + number2
```
This tells us, and the compiler, the `add` function takes two `Ints` and returns an `Int`.
Parentheses can be used to define function parameters (which are themselves defined with the same syntax, hence the need for parentheses).

```haskell
-- Transform every character in a string
map : (Char -> Char) -> String -> String
map charMapperFunction string =
    -- ...
```
This shows the `map` function takes a function (which itself takes a `Char` and returns a `Char`) followed by a `String`, and returns a `String`.

## Numbers

There are many types of numbers available in Haskell. For now, we will focus on `Int` and `Float` types.
`Int` corresponds to the set of positive and negative integers.
`Float` corresponds to the set of real numbers, limited by the precision of the computer.
Operations defined on numbers usually work on one type or the other, but not mixing them.
There are functions to convert between the two, such that `toFloat` which converts `Int` to `Float` and `round`, `floor`, `ceiling` or `truncate` which convert a `Float` to an `Int` with different semantics.

## Basic String Operations

To describe words and sentences in code, Haskell has a `String` type. In Haskell, strings are just lists of characters, two types we will learn more about later; for now, know that this allows us to concatenate strings with the list operator `++`.
String literals are written between double quotes such as `"Hello World!"`.
The concatenation operator `++` and equality operator `==` are available by default.

```haskell
hello : String -> String
hello subject = "Hello " ++ subject

hello "World!"      --> "Hello World!"
```
