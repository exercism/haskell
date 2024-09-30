# About

[Lists][list] are a basic data type in Haskell for holding a collection of values.
Lists are _immutable_, meaning they cannot be modified.
Any operation that changes a list returns a new list.
There are several methods in the prelude which allows you to work with Lists.

Lists in Haskell are implemented as [linked lists][linked-list-wiki], and not as arrays of contiguous memory location.
Therefore, accessing an element in a list takes linear time depending on the length of the list.

Lists can be written in literal form, head-tail notation, (which uses the `cons` operator `:`), or a combination of both:

```haskell
-- Literal Form
[]
[1]
[1, 2, 3]

-- Head-tail Notation
[]
-- same as [1]
1 : []
-- same as [1, 2, 3]
1 : (2 : (3 : []))

-- Mixed
-- same as [1, 2, 3]
1 : [2, 3]
```

Head-tail notation can be used to append items to a list.

```haskell
list = [2, 1]

[3, 2, 1] == 3 : list
-- -> True
```

Appending elements to a list during iteration is considered an anti-pattern. 
Appending an element requires walking through the entire list and adding the element at the end, therefore, appending a new element in each iteration would require walking through the entire list in each iteration.

We can achieve the same result by prepending an element to the reversed list, and then reversing the result. Prepending is a fast operation and requires constant time.

```haskell
-- Appending to the end of a list (potentially slow)
[1, 2, 3] ++ [4] ++ [5] ++ [6]

-- Prepend to the start of a list (faster, due to the nature of linked lists)
6 : (5 : (4 : [3, 2, 1]))
-- then reverse!
```

There are several common Prelude functions for lists:

- [`head`][head] returns the _head_ of a list -- the _first_ item in a list.
- [`tail`][tail] returns the _tail_ of the list -- the list _minus_ the _first_ item.
- [`length`][length] returns the number items in the list.
- [`elem`][in] returns a boolean value indicating whether the item is an element in the list.

There is also the [`List` module][list].

Lists can only contain one data type.

```haskell
list = [1, "string"]
-- Error: No instance for (Num String) arising from the literal ‘1’
```

## Type annotation

The type annotation of a list is `[a]` where a is the type which the lists holds, for example `String` or `Int`.

``` haskell
a :: [Int] 
a = [1, 2, 3]
```

[enum]: https://hexdocs.pm/elixir/Enum.html
[enum-protocol]: https://hexdocs.pm/elixir/Enumerable.html
[hd]: https://hexdocs.pm/elixir/Kernel.html#hd/1
[in]: https://hexdocs.pm/elixir/Kernel.html#in/2
[length]: https://hexdocs.pm/elixir/Kernel.html#length/1
[list]: https://hexdocs.pm/elixir/List.html
[stream]: https://hexdocs.pm/elixir/Stream.html
[tl]: https://hexdocs.pm/elixir/Kernel.html#tl/1
[linked-list-wiki]: https://en.wikipedia.org/wiki/Linked_list
