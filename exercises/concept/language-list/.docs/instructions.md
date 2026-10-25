# Instructions

In this exercise you need to implement some functions to manipulate a list of programming languages.

## 1. Define a function to return an empty language list

Define the `new` function that takes no arguments and returns an empty list.

```haskell
new
-- -> []
```

## 2. Define a function to add a language to the list

Define the `add/2` function that takes 2 arguments (a _language list_ and a string literal of a _language_).
It should return the resulting list with the new language prepended to the given list.

```haskell
add new "Clojure"
-- -> ["Clojure"]
add ["Clojure"] "Haskell"
-- -> ["Haskell", "Clojure"]
```

## 3. Define a function to remove a language from the list

Define the `remove` function that takes 1 argument (a _language list_).
It should return the list without the first item. Assume the list will always have at least one item.

```haskell
remove ["Haskell", "Clojure", "Erlang"]
-- -> ["Clojure", "Erlang"]
```

## 4. Define a function to return the first item in the list

Define the `first` function that takes 1 argument (a _language list_).
It should return the first language in the list.
Assume the list will always have at least one item.

```haskell
first ["Elixir", "Haskell", "Clojure", "Prolog"]
-- -> "Elixir"
```

## 5. Define a function to return how many languages are in the list

Define the `count` function that takes 1 argument (a _language list_). 
It should return the number of languages in the list.

```haskell
count ["Prolog", "Elm"]
-- -> 2
```

## 6. Define a function to determine if the list includes a functional language

Define the `isFunctionalList` function which takes 1 argument (a _language list_). 
It should return a boolean value. 
It should return `True` if _"Haskell"_ is one of the languages in the list.

```haskell
isFunctionalList ["Haskell"]
-- -> True
```
