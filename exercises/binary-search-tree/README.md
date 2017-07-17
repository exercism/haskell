# Binary Search Tree

Insert and search for numbers in a binary tree.

When we need to represent sorted data, an array does not make a good
data structure.

Say we have the array `[1, 3, 4, 5]`, and we add 2 to it so it becomes
`[1, 3, 4, 5, 2]` now we must sort the entire array again! We can
improve on this by realizing that we only need to make space for the new
item `[1, nil, 3, 4, 5]`, and then adding the item in the space we
added. But this still requires us to shift many elements down by one.

Binary Search Trees, however, can operate on sorted data much more
efficiently.

A binary search tree consists of a series of connected nodes. Each node
contains a piece of data (e.g. the number 3), a variable named `left`,
and a variable named `right`. The `left` and `right` variables point at
`nil`, or other nodes. Since these other nodes in turn have other nodes
beneath them, we say that the left and right variables are pointing at
subtrees. All data in the left subtree is less than or equal to the
current node's data, and all data in the right subtree is greater than
the current node's data.

For example, if we had a node containing the data 4, and we added the
data 2, our tree would look like this:

      4
     /
    2

If we then added 6, it would look like this:

      4
     / \
    2   6

If we then added 3, it would look like this

       4
     /   \
    2     6
     \
      3

And if we then added 1, 5, and 7, it would look like this

          4
        /   \
       /     \
      2       6
     / \     / \
    1   3   5   7

## Hints

To complete this exercise you need to create the data type `BST`,
with `Eq` and `Show` instances, and implement the functions:

- `bstLeft`
- `bstRight`
- `bstValue`
- `empty`
- `fromList`
- `insert`
- `singleton`
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

Josh Cheek [https://twitter.com/josh_cheek](https://twitter.com/josh_cheek)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
