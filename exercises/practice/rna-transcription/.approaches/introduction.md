# Introduction

This problem requires both

- validating that all input characters validly denote DNA nucleobases, and
- producing these DNA nucleobases' corresponding RNA nucleobases.

The first below listed approach has these tasks performed separately.
The other ones combine them in a single pass, in progressively more succinct ways.


## Approach: validate first, then transcribe

```haskell
toRNA :: String -> Either Char String
toRNA dna =
  case find (`notElem` "GCTA") dna of
    Nothing -> Right (map transcribe dna)
    Just c -> Left c
  where
    transcribe = \case
      'G' -> 'C'
      'C' -> 'G'
      'T' -> 'A'
      'A' -> 'U'
```

First search for the first invalid nucleobase.
If you find one, return it.
If all are valid, transcribe the entire strand in one go using `map`.

This approach has the input walked twice.
Other approaches solve this problem in one pass.

This solution deals with nucleobases twice: first when validating, and again when transcribing.
Ideally, nucleobases are dealt with in only one place in the code.

[Read more about this approach][validate-first].


## Approach: a single pass using only elementary operations

```haskell
toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (n : dna) = case transcribe n of
  Nothing -> Left n
  Just n' -> case toRNA dna of
    Left c -> Left c
    Right rna -> Right (n' : rna)

transcribe :: Char -> Maybe Char
transcribe = \case
  'G' -> Just 'C'
  'C' -> Just 'G'
  'T' -> Just 'A'
  'A' -> Just 'U'
  _ -> Nothing
```

This solution combines validation and transcription in a single list traversal.
It is _elementary_ in the sense that it employs no abstractions: it uses only constructors (`[]`, `(:)`, `Nothing`, `Just`, `Left`, `Right`) and pattern matching, and no predefined functions at all.

Some of the code patterns used in this solution are very common, and were therefore abstracted into standard library functions.
The approaches listed below show how much these functions can help to concisely express this approach's logic.

[Read more about this approach][elementary].


## Approach: use `do`-notation

```haskell
toRNA :: String -> Either Char String
toRNA [] = pure []
toRNA (n : dna) = do
  n' <- transcribe n
  rna <- toRNA dna
  pure (n' : rna)

transcribe :: Char -> Either Char Char
transcribe = \case
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  c -> Left c
```

The [elementary solution][elementary] displays a common pattern that can equivalently be expressed using the common monadic `>>=` combinator and its `do`-notation [syntactic sugar][wikipedia-syntactic-sugar].

[Read more about this approach][do-notation].


## Approach: use `Functor`/`Applicative` combinators

```haskell
toRNA :: String -> Either Char String
toRNA [] = pure []
toRNA (n : dna) = (:) <$> transcribe n <*> toRNA dna

transcribe :: Char -> Either Char Char
transcribe = \case
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  c -> Left c
```

The [elementary solution][elementary] displays a number of common patterns.
As demonstrated by the [`do` notation solution][do-notation], these can be expressed with the `>>=` operator.
However, the full power of `Monad` is not required.
The same logic can also be expressed using common functorial combinators such as `fmap`/`<$>` and `<*>`.

[Read more about this approach][functorial-combinators].


## Approach: use `traverse`

```haskell
toRNA :: String -> Either Char String
toRNA = traverse $ \case
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  n -> Left n
```

As it turns out, the [solution that uses functorial combinators][functorial-combinators] closely resembles the definition of `traverse` for lists.
In fact, through a series of rewritings it can be shown to be equivalent.

[Read more about this approach][traverse].


## General guidance

### Language extensions

For various reasons, some of GHC's features are locked behind switches known as _language extensions_.
You can enable these by putting so-called _language pragmas_ at the top of your file:

```haskell
-- This 👇 is a language pragma
{-# LANGUAGE LambdaCase #-}

module DNA (toRNA) where

{-
    The rest of your code here
-}
```


#### `LambdaCase`

Consider the following possible definition of `map`.

```haskell
map f xs = case xs of
  []     -> []
  x : xs' -> f x : map xs'
```

Here, a parameter `xs` is introduced only to be immediately pattern matched against, after which it is never used again.

Coming up with good names for such throwaway variables can be tedious and hard.
The `LambdaCase` extension allows us to avoid having to by providing an extra bit of [syntactic sugar][wikipedia-syntactic-sugar]:

```haskell
f = \case { }
-- is syntactic sugar for / an abbreviation of
f = \x -> case x of { }
```

The above definition of `map` can equivalently be written as

```haskell
map f = \case
  []     -> []
  x : xs -> f x : map f xs
```


[do-notation]:
    https://exercism.org/tracks/haskell/exercises/rna-transcription/approaches/do-notation
    "Approach: use do-notation"
[elementary]:
    https://exercism.org/tracks/haskell/exercises/rna-transcription/approaches/elementary
    "Approach: a single pass using only elementary operations"
[functorial-combinators]:
    https://exercism.org/tracks/haskell/exercises/rna-transcription/approaches/functorial-combinators
    "Approach: use Functor/Applicative combinators"
[traverse]:
    https://exercism.org/tracks/haskell/exercises/rna-transcription/approaches/traverse
    "Approach: use traverse"
[validate-first]:
    https://exercism.org/tracks/haskell/exercises/rna-transcription/approaches/validate-first
    "Approach: validate first"


[wikipedia-syntactic-sugar]:
    https://en.wikipedia.org/wiki/Syntactic_sugar
    "Wikipedia: Syntactic sugar"
