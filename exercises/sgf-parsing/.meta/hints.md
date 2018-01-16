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
