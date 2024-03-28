# Introduction

The **list** might well be Haskell's most important data structure.


## Syntax

Here are some examples of lists.

```haskell
someIntegers  = [3, -1, 4, 1, 5]    -- a list of integers
someDoubles   = [9.2, 6.53, 5.89]   -- a list of decimal numbers
someBools     = [True, False]       -- a list of booleans
someChars     = ['H', 'i', '!']     -- a list of characters
someFunctions = [isUpper, isSpace]  -- a list of functions
emptyList     = []                  -- an empty list
```
~~~~exercism/advanced
The above expressions are called **list literals**, because just like number literals (`9`, `1.6`) the syntax is special.
Less syntactically special ways of building lists exist.
We'll get to those when we learn about **algebraic data types** and **pattern matching**.
~~~~

For some types of elements, a shorthand for ranges is available:

```haskell
someOddNumbers = [9, 11 .. 17]         -- [9, 11, 13, 15, 17]
firstHalfOfAlphabet = ['a' .. 'm']     -- "abcdefghijklm"
everySecondLetter = ['b', 'd' .. 'z']  -- "bdfhjlnprtvxz"
multiplesOf7 = [0, 7 ..]   -- infinite list [0, 7, 14, 21, …]
positiveIntegers = [1 ..]  -- infinite list [1, 2, 3, …]
```

~~~~exercism/caution
Do not ask your computer to print an infinite list, or to otherwise process all its elements.
Even though much faster than you, it'll never finish.

Asking for a finite amount of work is fine though:

```haskell
ghci> take 5 [0, 3 ..]
[0,3,6,9,12]
```
~~~~


## List types

In Haskell, lists are **homogenous**.
This means that all elements of a list have the same type.
When you try to put elements of different types into the same list, you will get a type error.

```haskell
heterogenousList = [True, 'X']
-- […]: error:
--     • Couldn't match expected type ‘Bool’ with actual type ‘Char’
--     • In the expression: 'X'
```

Because elements's types are always the same, it makes sense to speak of _lists of integers_, _lists of booleans_, _lists of functions_, etc.
Indeed, all of these kinds of lists have their own types!

```haskell
ghci> :type someBools
someBools :: [Bool]  -- the type of lists of booleans
ghci> :type someChars
someChars :: [Char]  -- the type of lists of characters
ghci> :type someFunctions
someFunctions :: [Char -> Bool]  -- the type of lists of functions
```

~~~~exercism/note
As you can see, the `[…]` syntax is slightly overloaded: it can denote both values and types.
It is however never truly ambiguous, because of strictly distinguished syntactical contexts:

```haskell
name :: {- only types here -}
name = {- only values here -}
```

To add to that,

- `[a, b, c]` and `[]` are values
- `[6]`, `[True]`, `[[]]` are values, because `6`, `True`, `[]` are all values
- `[Bool]` is a type, because `Bool` is a type
- `[a]` is a type when `a` is a type variable, and a value otherwise.
~~~~

~~~~exercism/advanced
The family of types of lists has a name: `[]`.
It is, strictly speaking, not a type itself.
Instead, it is a _type constructor_, or _parametric type_.
We'll get to what this means exactly in subsequent concept nodes.

When you hear people talk about «the `[]` type», most likely they mean this family of types rather than any specific type.
~~~~


## Lists representing text: `String`

Haskell does not have a built-in data type for representing text.
Instead, by default it represents text as lists of characters.

```haskell
['H', 'i', '!']  -- a greeting: a list of characters
```

Lists of characters are syntactically unwieldy, so Haskell features some handy syntactic sugar:

```haskell
ghci> ['H', 'i', '!'] == "Hi!"
True
ghci> "" == []
True
```

~~~~exercism/note
In contrast to in some other languages, in Haskell single quotes (`'`) and double quotes (`"`) are not interchangeable.
Single quotes denote `Char`acters, whereas double quotes denote `String`s (i.e. lists of characters).

```haskell
ghci> :type 'a'
'a' :: Char
ghci> :type "a"
"a" :: String
```
~~~~

Also for convenience, Haskell provides the name `String`, which is a _type synonym_ of `[Char]`.
Two names for exactly the same type, they are entirely interchangeable:

```haskell
ghci> (['H', 'i', '!'] :: String) == ("Hi!" :: [Char])
True
```


## Working with lists

Owing to lists' ubiquitousness, lots of utilities for working with them are present in the standard library.

The very most important functions you'll find in [the `Prelude`][prelude].
Other much-used functions live in [the `Data.List` module][data.list].
Be sure to have a good look-around!

Functions that are provided by the `Prelude` you do not need to import, as the `Prelude` itself is implicitly imported by default.
Functions from `Data.List` however you do need to explicitly import.

~~~~exercism/advanced
Haskell's lists are singly-linked lists.
This has consequences in performance characteristics of list operations.
For example, `length` takes time proportional to list length, `!!` (index) takes time linear in the index, and `++` (append) takes time proportional to (only!) the length of the left operand.
~~~~


### How to interpret `Foldable` constraints

In many places in the documentation you'll find functions with signatures mentioning `Foldable`, e.g.

```haskell
length :: Foldable t => t a -> Int
```

We'll get to what this means exactly later.
For now, you can read all such signatures as if they are about lists:

```haskell
--                1. remember this name
--                👇
length :: Foldable t => t a -> Int
--                      👆
--                      2. replace all occurrences with […]
--           👆
--           3. remove constraint

-- Result:
length :: [a] -> Int
```


[data.list]: https://hackage.haskell.org/package/base/docs/Data-List.html "Data.List documentation"
[prelude]: https://hackage.haskell.org/package/base/docs/Prelude.html "Prelude documentation"
