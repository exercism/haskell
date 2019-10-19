## Hints

You need to implement the `diamond` function which prints a diamond starting at
`A` with the given character at its widest points. You can use the provided
signature if you are unsure about the types, but don't let it restrict your
creativity:

```haskell
diamond :: Char -> Maybe [String]
```

This exercise works with textual data. For historical reasons, Haskell's
`String` type is synonymous with `[Char]`, a list of characters. For more
efficient handling of textual data, the `Text` type can be used.

As an optional extension to this exercise, you can

- Read about [string types](https://haskell-lang.org/tutorial/string-types) in
  Haskell.
- Add `- text` to your list of dependencies in package.yaml.
- Import `Data.Text` in [the following
  way](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c):

```haskell
import qualified Data.Text as T
import           Data.Text (Text)
```

- You can now write e.g. `diamond :: Char -> Maybe [Text]` and refer to
  `Data.Text` combinators as e.g. `T.pack`,
- Look up the documentation for
  [`Data.Text`](https://hackage.haskell.org/package/text/docs/Data-Text.html),
- You can then replace all occurrences of `String` with `Text` in Diamond.hs:

```haskell
diamond :: Char -> Maybe [Text]
```

This part is entirely optional.
