module Sgf (parseSgf) where

import Data.Map  (Map)
import Data.Text (Text)
import Data.Tree (Tree)

-- You may find it useful to use the following type
-- definitions for SgfTree and SgfNode.
--
-- A tree of nodes.
-- type SgfTree = Tree SgfNode
--
-- A node is a property list, each key can only occur once.
-- type SgfNode = Map Text [Text]
--
-- Keys may have multiple values associated with them.
--
-- The parsec library is part of the Haskell Platform. Please use it to
-- your advantage.

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf = undefined
