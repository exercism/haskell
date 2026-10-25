# Hints

## General

- Use the built-in [(linked) list type][list].

## 1. Define a function to return an empty language list

- The function needs to return `[]`.

## 2. Define a function to add a language to the list

- An element can be prepended to a list using `:`.

## 3. Define a function to remove a language from the list

- Haskell [provides a function][tail] to return a list with the first item removed.

## 4. Define a function to return the first item in the list

- HaskellHaskell [provides a function][head] to get the first item from a list.

## 5. Define a function to return how many languages are in the list

- Haskell [provides a function][length] to count the length of a list.

## 6. Define a function to determine if the list includes a functional language

- Your function should return a boolean value indicating whether `"Haskell"` is a member of the list. 
  Haskell [provides a function][elem] to test list membership.

[list]: https://hackage.haskell.org/package/base/docs/Data-List.html
[head]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:head
[tail]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:tail
[elem]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:elem
[length]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:length
