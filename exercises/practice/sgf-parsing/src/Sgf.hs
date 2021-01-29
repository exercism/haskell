module Sgf (parseSgf) where

import Data.Map  (Map)
import Data.Text (Text)
import Data.Tree (Tree)

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = error "You need to implement this function."
