# Introduction

Suppose you are on Mercury.
The [orbital period][wikipedia-orbital-period] of Mercury is approximately one fourth of an Earth year.
This means that in one Earth year, Mercury will complete about four whole orbits around the sun.
As a consequence, your 'age' on Mercury is about four times your age on Earth.

More generally, your 'age' on a certain planet is the number of seconds since your birth, divided by the number of seconds it takes the planet to orbit the sun once.

The various planets' orbital periods as compared to the Earth's are given.
To convert them from _Earth years_ per orbit to _seconds_ per orbit you need to multiply this by the number of seconds in one Earth year.


## Approach: give meaningful names to important values

```haskell
ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (periodInEarthYears * secondsPerEarthYear)
  where
    secondsPerEarthYear = 60 * 60 * 24 * 365.25

    periodInEarthYears = case planet of
      Mercury -> 0.2408467
      Venus -> 0.61519726
      Earth -> 1
      Mars -> 1.8808158
      Jupiter -> 11.862615
      Saturn -> 29.447498
      Uranus -> 84.016846
      Neptune -> 164.79132
```

This approach uses a `case` expression to choose the relevant orbital period.
Also a `where` clause is used to give names to important values.
This tends to greatly improve readability.


## General guidance

### `where` clauses are your friend!

Giving meaningful names to subexpressions can do wonders for code readability.
`let` expressions allow the same.
Being expressions &ndash; which `where` clauses aren't &ndash; `let` expressions are a bit more flexible in their use.
However, `where` clauses list the local definitions _after_ the main expression.
This allows you to paint the broad strokes of your strategy first, and to fill in the details later.
This is so convenient that it amply compensates for `where` clauses not being expressions.

More on `where` and `let` elsewhere:

- Haskell Wiki: [Let vs. Where][haskellwiki-let-vs-where]
- Haskell Wikibook:
  - [`where` clauses][wikibook-where]
  - [`let` bindings][wikibook-let]
  - [`let` and `where` revisited][wikibook-let-vs-where]


### `case` expressions are also your friend!

Many beginning Haskellers write code like

```haskell
ageOn planet seconds
  | planet == Mercury = _
  | planet == Venus   = _
  | {- etc. -}        = _
```

This is an [anti-pattern][wikipedia-anti-pattern].
Pattern-match instead, for example using a `case` expression:

```haskell
ageOn planet seconds =
  case planet of
    Mercury    -> _
    Venus      -> _
    {- etc. -} -> _
```

Pattern matching is a fundamental concept in Haskell, but this document is too short to be able to fully explain it.
Please consult your other learning resources.

Pattern matching with `case` has benefits over using guards:

- `case` expressions are _expressions_.
  Therefore as pieces of code they are very easy to move around during code composition.
  You can give them names (e.g. in a `where` clause) and also pass them to functions as arguments.
- The compiler is able to check that you handle all possible cases.
  If you overlook some cases and use guards, the compiler will not help you.
  But if you are pattern matching it will!
- When you use pattern matching, the compiler can use its understanding of your code to apply code transformations that improve performance.


[haskellwiki-let-vs-where]:
    https://wiki.haskell.org/Let_vs._Where
    "Haskell Wiki: Let vs. Where"
[wikibook-let-vs-where]:
    https://en.wikibooks.org/wiki/Haskell/More_on_functions#let_and_where_revisited
    "Haskell Wikibook: let and where revisited"
[wikibook-let]:
    https://en.wikibooks.org/wiki/Haskell/Next_steps#let_bindings
    "Haskell Wikibook: let bindings"
[wikibook-where]:
    https://en.wikibooks.org/wiki/Haskell/Variables_and_functions#where_clauses
    "Haskell Wikibook: where clauses"
[wikipedia-anti-pattern]:
    https://en.wikipedia.org/wiki/Anti-pattern
    "Wikipedia: Anti-pattern"
[wikipedia-orbital-period]:
    https://en.wikipedia.org/wiki/Orbital_period
    "Wikipedia: Orbital period"
