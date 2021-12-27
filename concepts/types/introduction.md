# Haskell Types

Every value has an associated type (Intuitively, we can think of types as sets of values). Examples of expressions include atomic values such as the integer `5`, the character `a`, and the function `inc x = x+1`, as well as structured values such as the list `[1,2,3]` and the pair `('b',4)`.

Types in a sense describe values, and the association of a value with its type is called a typing. Using the examples of values and types above, we write typings as follows:

```haskell
     5  :: Int
    'a' :: Char
    inc :: Int -> Int
[1,2,3] :: [Int]
('b',4) :: (Char,Int)
```
The `::` can be read "has type."

## Type Annotations

Type annotations for functions are defined with `name : parameter types -> return type`, parameter types also being separated by `->`.

```haskell
add :: Int -> Int -> Int
add number1 number2 = number1 + number2
```
This tells us, and the compiler, the `add` function takes two `Int` and returns an `Int`.
Parentheses can be used to define function parameters (which are themselves defined with the same syntax, hence the need for parentheses).

```haskell
-- Transform every character in a string
map :: (Char -> Char) -> String -> String
map charMapperFunction string =
    -- ...
```
This shows the `map` function takes a function (which itself takes a `Char` and returns a `Char`) followed by a `String`, and returns a `String`.

Type annotations can also indicate the function takes more than one possible type as a parameter. That is, it is polymorphic. To show polymorphism we use a type variable. For example, the function `length` takes a list (`[]`) of any type `a` and returns an `Int` with the number of elements in the list.  

```haskell
length :: [a] -> Int
```

Polymorphism is a more advanced concept we will return to later. For now, remember a variable in a type declaration means the function can take more than one type.