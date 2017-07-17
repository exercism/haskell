# Simple Linked List

Write a simple linked list implementation that uses Elements and a List.

The linked list is a fundamental data structure in computer science,
often used in the implementation of other data structures. They're
pervasive in functional programming languages, such as Clojure, Erlang,
or Haskell, but far less common in imperative languages such as Ruby or
Python.

The simplest kind of linked list is a singly linked list. Each element in the
list contains data and a "next" field pointing to the next element in the list
of elements.

This variant of linked lists is often used to represent sequences or
push-down stacks (also called a LIFO stack; Last In, First Out).

As a first take, lets create a singly linked list to contain the range (1..10),
and provide functions to reverse a linked list and convert to and from arrays.

When implementing this in a language with built-in linked lists,
implement your own abstract data type.

## Hints

To complete this exercise, you need to create the data type `LinkedList`,
and implement the following functions:

- `datum`
- `fromList`
- `isNil`
- `new`
- `next`
- `nil`
- `reverseLinkedList`
- `toList`

You will find a dummy data declaration and type signatures already in place,
but it is up to you to define the functions and create a meaningful data type,
newtype or type synonym.



## Getting Started

For installation and learning resources, refer to the
[exercism help page](http://exercism.io/languages/haskell).

## Running the tests

To run the test suite, execute the following command:

```bash
stack test
```

#### If you get an error message like this...

```
No .cabal file found in directory
```

You are probably running an old stack version and need
to upgrade it.

#### Otherwise, if you get an error message like this...

```
No compiler found, expected minor version match with...
Try running "stack setup" to install the correct GHC...
```

Just do as it says and it will download and install
the correct compiler version:

```bash
stack setup
```

## Running *GHCi*

If you want to play with your solution in GHCi, just run the command:

```bash
stack ghci
```

## Feedback, Issues, Pull Requests

The [exercism/haskell](https://github.com/exercism/haskell) repository on
GitHub is the home for all of the Haskell exercises.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Source

Inspired by 'Data Structures and Algorithms with Object-Oriented Design Patterns in Ruby', singly linked-lists. [http://www.brpreiss.com/books/opus8/html/page96.html#SECTION004300000000000000000](http://www.brpreiss.com/books/opus8/html/page96.html#SECTION004300000000000000000)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
