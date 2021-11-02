module Anagram (anagramsFor) where

import Data.Function  (on)
import Data.List      (sort)
import Data.Set       (Set, filter)
import Data.Text      (Text, toLower, unpack)
import Prelude hiding (filter)

anagramsFor :: Text -> Set Text -> Set Text
anagramsFor x = filter (\w -> x `isDistinctOf` w && x `isAnagramOf` w)
  where
    isDistinctOf = (/=) `on` toLower
    isAnagramOf  = (==) `on` sort . unpack . toLower
