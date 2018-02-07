# SGF Parsing

Parsing a Smart Game Format string.

[SGF](https://en.wikipedia.org/wiki/Smart_Game_Format) is a standard format for
storing board game files, in particular go.

SGF is a fairly simple format. An SGF file usually contains a single
tree of nodes where each node is a property list. The property list
contains key value pairs, each key can only occur once but may have
multiple values.

An SGF file may look like this:

```text
(;FF[4]C[root]SZ[19];B[aa];W[ab])
```

This is a tree with three nodes:

- The top level node has two properties: FF\[4\] (key = "FF", value =
  "4") and C\[root\](key = "C", value = "root"). (FF indicates the
  version of SGF and C is a comment.)
  - The top level node has a single child which has a single property:
    B\[aa\].  (Black plays on the point encoded as "aa", which is the
    1-1 point (which is a stupid place to play)).
    - The B\[aa\] node has a single child which has a single property:
      W\[ab\].

As you can imagine an SGF file contains a lot of nodes with a single
child, which is why there's a shorthand for it.

SGF can encode variations of play. Go players do a lot of backtracking
in their reviews (let's try this, doesn't work, let's try that) and SGF
supports variations of play sequences. For example:

```text
(;FF[4](;B[aa];W[ab])(;B[dd];W[ee]))
```

Here the root node has two variations. The first (which by convention
indicates what's actually played) is where black plays on 1-1. Black was
sent this file by his teacher who pointed out a more sensible play in
the second child of the root node: `B[dd]` (4-4 point, a very standard
opening to take the corner).

A key can have multiple values associated with it. For example:

```text
(;FF[4];AB[aa][ab][ba])
```

Here `AB` (add black) is used to add three black stones to the board.

There are a few more complexities to SGF (and parsing in general), which
you can mostly ignore. You should assume that the input is encoded in
UTF-8, the tests won't contain a charset property, so don't worry about
that. Furthermore you may assume that all newlines are unix style (`\n`,
no `\r` or `\r\n` will be in the tests) and that no optional whitespace
between properties, nodes, etc will be in the tests.

The exercise will have you parse an SGF string and return a tree
structure of properties. You do not need to encode knowledge about the
data types of properties, just use the rules for the
[text](http://www.red-bean.com/sgf/sgf4.html#text) type everywhere.

## Hints

The Sgf module should export a parseSgf module with the following signature:

```haskell
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
```

You may find it useful to copy the following definitions for SgfTree
and SgfNode:

```haskell
-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]
```

The parsec library is part of the Haskell Platform. Please use it to
your advantage.



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

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
