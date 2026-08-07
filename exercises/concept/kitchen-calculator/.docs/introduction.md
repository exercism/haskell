## Tuple

[Tuples][tuple] are used commonly to group information, they differ from lists in that they are fixed-size and can hold different data types.
Tuples are created using curly braces, `()`, and are often used to return multiple values from a function.

```haskell
tuple = (1, "abc", False)
```

This can be useful when you for example want to store a coordinate in a 2D space, then you know that the tuple will always have two elements, the x and y coordinate.

```haskell
coordinate = (3, 4)
```

### Accessing elements

Quite often you work with short tuples, and you can access the elements using the `fst` and `snd` functions.

```haskell
x = fst coordinate
-- x = 3
y = snd coordinate
-- y = 4
```

## Pattern matching

When writing Haskell functions, we can make use of an [assertive style][assertive-style] with [pattern matching][pattern-match-doc]:
Pattern matching is a very powerful feature of Haskell that allows you to match on data constructors and bind variables to the values inside.
As the name suggests, what we do is match values against patterns, and if the value matches the pattern, we can bind variables to the values inside the pattern.
Pattern matching is mainly built of these concepts: recognizing values, binding variables, and breaking down values.

Take this function for example:

```haskell
lucky :: Int -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck, pal!"
```

Here we have a function `lucky` that takes an `Int` and returns a `String`.
We have defined two patterns for the function, one that matches the number `7` and one that matches any other number, the name can be anything (as long as it follows Haskell naming convention), but we use `x` here.
If the number is `7`, the function will return `"Lucky number seven!"`, otherwise it will return `"Sorry, you're out of luck, pal!"`.
What is important to note here is that the patterns are checked from top to bottom, so if we had swapped the order of the patterns, the function would always return `"Lucky number seven!"`.

### List patterns

A very common pattern is to match on lists, and that is taking the head and the tail of the list.
This is due to lists nature of being a linked list.
Here is an example of a function that returns the head and the tail of a list:

```haskell
headAndTail :: [Int] -> (Int, [Int])
headAndTail [] = error "Can't call head on an empty list"
headAndTail (x:xs) = (x, xs)
```

We have two patterns here, one that matches an empty list and one that matches a list with at least one element.
This is due to if the list is empty, we need to have a case for that, otherwise we would get a runtime error.
If the list is not empty, we can match the head of the list with `x` and the tail of the list with `xs`.
This is done using the `:` (cons) operator, which is used to prepend an element to a list.
But in pattern matching it allows us to break down a list into its head and tail, so in a way doing the opposite.

The `xs` is a common name for the tail of a list, it highlights that it is a list, but if you would be working with a nested list, you could use `xss` to highlight that it is a list of lists.

### Tuple patterns

As with lists, we can also match on tuples.
Here is an example of a function that takes a tuple and returns the first and second element:

```haskell
sumTuple :: (Int, Int) -> Int
sumTuple (x, y) = x + y
```

Here we have a pattern that matches a tuple with two elements, the first element is bound to `x` and the second element is bound to `y`.

### Wildcard patterns

Sometimes we don't care about the value of a variable, we just want to match on the pattern.
This is where the wildcard pattern comes in, it is denoted by an underscore `_`.

Here is an example of a function that returns the first element of a list:

```haskell
head' :: [Int] -> Int
head' [] = error "Can't call head on an empty list"
head' (x:_) = x
```

Here we say we don't need the tail of the list, so we use the wildcard pattern to ignore it.

### Type patterns

We can also match on types, this is done by using the constructor of said type.
We can also extract values from the type, like we did with tuples.

```haskell
data AccountType = Guest | User String 

greet :: AccountType -> String
greet Guest = "Welcome, guest!"
greet (User name) = "Welcome, " ++ name ++ "!"
```

In the first pattern we match on the `Guest` constructor, and in the second pattern we match on the `User` constructor and bind the value inside to `name`.

[tuple]: https://hackage.haskell.org/package/base/docs/Data-Tuple.html
