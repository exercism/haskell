# Hints

This exercise works with textual data. For historical reasons, Haskell's
`String` type is synonymous with `[Char]`, a list of characters. For more
efficient handling of textual data, the `Text` type can be used.

As an optional extension to this exercise, you can

- Read about [string types](https://haskell-lang.org/tutorial/string-types) in Haskell.
- Add `- text` to your list of dependencies in package.yaml.
- Import `Data.Text` in [the following way](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c):

        import qualified Data.Text as T
        import           Data.Text (Text)

- You can now write e.g. `abbreviate :: Text -> Text` and refer to `Data.Text` combinators as e.g. `T.filter`.
- Look up the documentation for [`Data.Text`](https://hackage.haskell.org/package/text/docs/Data-Text.html).

This part is entirely optional.
