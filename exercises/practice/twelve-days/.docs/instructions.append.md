# Hints

You need to implement the `recite` function which outputs the lyrics to
'The Twelve Days of Christmas'. You can use the provided signature
if you are unsure about the types, but don't let it restrict your creativity.

This exercise works with textual data. For historical reasons, Haskell's
`String` type is synonymous with `[Char]`, a list of characters. For more
efficient handling of textual data, the `Text` type can be used.

As an optional extension to this exercise, you can

- read about [string types](https://haskell-lang.org/tutorial/string-types) in
  Haskell.
- add `- text` to your list of dependencies in package.yaml.
- import `Data.Text` in [the following
  way](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c):

```haskell
import qualified Data.Text as T
import           Data.Text (Text)
```

- write e.g. `recite :: Int -> Int -> [Text]` and refer to
  `Data.Text` combinators as e.g. `T.pack`.
- look up the documentation for
  [`Data.Text`](https://hackage.haskell.org/package/text/docs/Data-Text.html).
- replace all occurrences of `String` with `Text` in TwelveDays.hs, i.e.:

```haskell
recite :: Int -> Int -> [Text]
```

This part is entirely optional.
