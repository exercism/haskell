# Using `Text`

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


## Using dependencies

The `Text` type and associated functions live in the `Data.Text` module of the external `text` package.
To be able to use it, you need to add this package to the list of dependencies in `package.yaml`:

```yaml
dependencies:
  - base
  - text  # 👈 Add this line
```

Thereafter you can import functions as you would normally:

```haskell
-- allow using the following names by themselves
import Data.Text (Text, strip, unsnoc)

-- require all other names from `Data.Text` to be prefixed with `Text.`
import qualified Data.Text as Text
```


## Language extensions

For various reasons, some of GHC's features are locked behind switches known as _language extensions_.
You can enable these by putting so-called _language pragmas_ at the top of your file:

```haskell
-- These 👇 are language pragmas
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Bob (responseFor) where

{-
    The rest of your code here
-}
```


### `OverloadedStrings`

By default, `"abc"` is interpreted by the compiler as denoting a `String`.
To get a `Text` value instead, you need to explicitly convert using `Text.pack`:

```haskell
someString :: _  -- compiler infers `String`
someString = "abc"

someText :: _  -- compiler infers `Text`
someText = Text.pack "abc"
```

This is a bit inconvenient.
The `OverloadedStrings` extension allows you to use string literals for `Text` as well.

```haskell
-- This only works with OverloadedStrings enabled
someText :: Text
someText = "abc"
```


### `ViewPatterns`

Recall, patterns occur in the following positions:

```haskell
-- in `case` expressions
_ = case expression of
  pattern -> expression

-- in function definition syntactic sugar
name pattern pattern = expression
```

The most common kinds of patterns are

```haskell
_ = case e of
  x       -> _  -- binding pattern
  Nothing -> _  -- constructor pattern
  _       -> _  -- wildcard pattern
```

The `ViewPattern` extension adds the _view pattern_ to the language, which has the form `expression -> pattern`.
When a value is matched against it, first the `expression` is applied to it as a function, and then the result of this is matched against the `pattern`.
One of its uses is the 'pre-processing' of arguments before pattern matching on them.

```haskell
minimum :: Ord a => [a] -> Maybe a
minimum (sort -> xs) =
  case xs of
    [] -> Nothing
    x : _ -> Just x
```

This implementation of `minimum` first `sort`s its argument, before pattern matching on it to retrieve the first element (if present).

Another way of writing this function, without the view pattern, is

```haskell
minimum :: Ord a => [a] -> Maybe a
minimum unsorted =
  let xs = sort unsorted
   in case xs of
        [] -> Nothing
        x : _ -> Just x
```

This is a bit more verbose.
But more importantly it introduces an extra name: `unsorted`.
We intend to use it only once (to `sort` it), but we might accidentally use it in more places.
By eliminating the need for an extra name, the view pattern makes it impossible to make this mistake.

The solution highlighted above uses a view pattern to remove initial and trailing spaces from the input.
That way, `isSilent` and `isQuestion` need not do it themselves anymore, or the introduction of an otherwise unnecessary name (for the stripped query) is avoided.
