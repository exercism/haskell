# Introduction

This problem requires checking against a series of conditions, each associated with a desired output.
This suggests using guards.


## General guidance

### Beware partial functions

This problem might tempt you into reaching for `last`.
However, you should know that it is dangerous:

```text
ghci> tail []
*** Exception: Prelude.tail: empty list
```

A crash!
That's no good.

Functions that do not return when given certain arguments (e.g. because they crash or get stuck in a loop) are known as [partial functions][wiki-partial-functions].
These functions display behavior (e.g. crashing) that is not documented in their types, which makes reasoning about code that uses them more difficult.
For this reason, strive to avoid partial functions.
It is almost always fairly easy to do so.


### Avoid duplicating code

At multiple points in your solution you'll need to know whether the query is a question, or yelled.
Do not copy&ndash;paste the code for determining this.
Instead, give meaningful names to these computations and then use these names to determine whether the query is a yelled question.

Likewise, if you strip the query, do so only once.


### `where` clauses are your friend!

Giving meaningful names to subexpressions can do wonders for code readability.
`where` clauses allow you to list local definitions at the end of the declaration.
This allows you to paint the broad strokes of your strategy first, and to fill in the details later.
Defined values are computed lazily, so you do not need to worry about accidentally performing expensive but unnecessary computations.

More on `where` elsewhere:

- Haskell Wikibook: [`where` clauses][wikibook-where]
- Learn You A Haskell: [Syntax in Functions &ndash; Where!?][lyah-where]
- Haskell Wiki: [Let vs. Where][haskellwiki-let-vs-where]


### When possible, consider turning functions into constants

Many beginning Haskellers write code like

```haskell
responseFor query
  | isSilent query = "Fine. Be that way!"
  | isQuestion query && isYelled query = "Calm down, I know what I'm doing!"
  | isQuestion query = "Sure."
  | isYelled query = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isSilent xs = _
    isQuestion xs = _
    isYelled xs = _
```

All three of the functions `isSilent`, `isQuestion`, and `isYelled` here are only ever given the same argument: `query`.
Therefore, we could replace their parameters with `query` in their bodies and it wouldn't make a difference.
That is, supposing for example

```haskell
isSilent :: String -> Bool
isSilent xs = all isSpace xs
```

we could change it into

```haskell
isSilent :: String -> Bool
isSilent xs = all isSpace query
```

and the result would be exactly the same, as `xs` would always be `query` anyway.

But then `isSilent` would become a function of one argument that it doesn't actually use.
We might as well remove the unused parameter and thereby turn the function into a constant.

```haskell
isSilent :: Bool
isSilent = all isSpace query
```

This has a nice effect on the readability of the surrounding code:

```haskell
responseFor query
  | isQuestion && isYelled = "Calm down, I know what I'm doing!"
  -- vs.
  | isQuestion query && isYelled query = "Calm down, I know what I'm doing!"
  where
    isQuestion = _
    isYelled = _
```


## Approach: using `String`

```haskell
responseFor :: String -> String
responseFor query
  | isSilent = "Fine. Be that way!"
  | isQuestion && isYelled = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isYelled = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isSilent = all isSpace query
    isQuestion = lastMay (filter (not . isSpace) query) == Just '?'
    isYelled = any isLetter query && not (any isLower query)
```

This solution uses `any` and `all` to determine whether the query consists entirely of whitespace, and whether all letters are uppercase.
It also eschews `last`, which is [partial][wiki-partial-functions], in favor of the safe alternative `lastMay`.

[Read more about this approach][string].


## Approach: using `Text`

```haskell
responseFor :: Text -> Text
responseFor (strip -> query)
  | isSilent = "Fine. Be that way!"
  | isQuestion && isYelled = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isYelled = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isSilent = Text.null query
    isQuestion = (snd <$> unsnoc query) == Just '?'
    isYelled = Text.any isLetter query && not (Text.any isLower query)
```

`String` is a very simple but inefficient representation of textual data.
This solution works with `Text` instead, which is a data type designed specifically for working with text.
It also employs a _view pattern_.

[Read more about this approach][text].


[string]:
    https://exercism.org/tracks/haskell/exercises/bob/approaches/string
    "Approach: using String"
[text]:
    https://exercism.org/tracks/haskell/exercises/bob/approaches/text
    "Approach: using Text"


[haskellwiki-let-vs-where]:
    https://wiki.haskell.org/Let_vs._Where
    "Haskell Wiki: Let vs. Where"
[lyah-where]:
    https://learnyouahaskell.github.io/syntax-in-functions.html#where
    "Syntax in Functions - Learn You a Haskell for Great Good!"
[wiki-partial-functions]:
    https://wiki.haskell.org/Partial_functions
    "Haskell Wiki: Partial functions"
[wikibook-where]:
    https://en.wikibooks.org/wiki/Haskell/Variables_and_functions#where_clauses
    "Haskell Wikibook: where clauses"
