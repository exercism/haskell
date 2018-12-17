## Hints

You need to implement the `responseFor` function that returns Bob's response
for a given input. You can use the provided signature if you are unsure
about the types, but don't let it restrict your creativity:

```haskell
responseFor :: String -> String
```

This exercise works with textual data. For historical reasons, Haskell's
`String` type is synonymous with `[Char]`, a list of characters. For more
efficient handling of textual data, the `Text` type can be used.

As an optional extension to this exercise, you can

- Read about [string types](https://haskell-lang.org/tutorial/string-types) in Haskell.
- Add `- text` to your list of dependencies in package.yaml.
- Import `Data.Text` in [the following way](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c):

      import qualified Data.Text as T
      import           Data.Text (Text)

- You can now write e.g. `responseFor :: Text -> Text` and refer to `Data.Text` combinators as e.g. `T.isSuffixOf`.
- Look up the documentation for [`Data.Text`](https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html),
- You can then replace all occurrences of `String` with `Text` in Bob.hs:

      ```haskell
      responseFor :: Text -> Text
      ```

This part is entirely optional.
