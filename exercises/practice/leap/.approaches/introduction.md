# Introduction

There are various idiomatic approaches to solve Leap, such as

- constructing [a logical expression][logical-expression],
- using [guards][guards], and
- using [a conditional expression][conditional-expression].


## General guidance

The key to determining whether a given year is a leap year is to know whether the year is evenly divisible by `4`, `100`, and `400`.
For determining that, you can use the [`mod` function][mod-function], which yields the remainder after division.

Operators are generally written _infix_: in `a + b`, the `+` is written _between_ its two operands.
In contrast, 'regular' functions in Haskell are generally written _prefix_: in `f x`, the function is written _before_ its argument.
Sometimes however it would be nicer to write a function between its two arguments.
This is possible when you surround the function name with back ticks: ``7 `div` 3`` means exactly the same as `div 7 3`, but is much nicer to read.

`where` clauses are your friend! Giving meaningful names to subexpressions can do wonders for code readability. `let` expressions allow the same. Being expressions &ndash; which `where` clauses aren't &ndash; `let` expressions are a bit more flexible in their use. However, `where` clauses list the local definitions _after_ the main expression. This allows you to paint the broad strokes of your strategy first, and to fill in the details later. This is so convenient that it amply compensates for `where` clauses not being expressions.

More on `where` and `let` elsewhere:

- Haskell Wiki: [Let vs. Where][haskellwiki-let-vs-where]
- Haskell Wikibook:
  - [`where` clauses][wikibook-where]
  - [`let` bindings][wikibook-let]
  - [`let` and `where` revisited][wikibook-let-vs-where]


## Which approach to use?

Code exists primarily for humans to read and reason about. Therefore, in general, go with the approach that _makes the most sense_.

All approaches listed here are valid choices unless marked otherwise. Be sure to check out all of their individual pages though: they each have interesting things to say!


[conditional-expression]:
    https://exercism.org/tracks/python/exercises/leap/approaches/conditional-expression
    "Approach: a conditional expression"
[guards]:
    https://exercism.org/tracks/python/exercises/leap/approaches/guards
    "Approach: a sequence of guards"
[logical-expression]:
    https://exercism.org/tracks/python/exercises/leap/approaches/logical-expression
    "Approach: a logical expression"


[wikibook-let-vs-where]:
    https://en.wikibooks.org/wiki/Haskell/More_on_functions#let_and_where_revisited
    "Haskell Wikibook: let and where revisited"
[wikibook-let]:
    https://en.wikibooks.org/wiki/Haskell/Next_steps#let_bindings
    "Haskell Wikibook: let bindings"
[wikibook-where]:
    https://en.wikibooks.org/wiki/Haskell/Variables_and_functions#where_clauses
    "Haskell Wikibook: where clauses"
[mod-function]:
    https://hackage.haskell.org/package/base/docs/Prelude.html#v:mod
    "Documentation of `mod` in the Prelude"
[haskellwiki-let-vs-where]:
    https://wiki.haskell.org/Let_vs._Where
    "Haskell Wiki: Let vs. Where"
