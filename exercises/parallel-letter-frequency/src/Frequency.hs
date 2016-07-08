module Frequency (frequency) where

import Data.Map  (Map)
import Data.Text (Text)

-- | Given a number workers to use in parallel and a list of
-- texts, returns the total frequency of each letter in the text.
frequency :: Int -> [Text] -> Map Char Int
frequency = undefined
