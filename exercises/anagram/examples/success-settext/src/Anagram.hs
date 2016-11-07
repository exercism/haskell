module Anagram (anagramsFor) where

import Data.Function  (on)
import Data.MultiSet  (fromList)
import Data.Set       (Set, filter)
import Data.Text      (Text, toLower, unpack)
import Prelude hiding (filter)

anagramsFor :: Text -> Set Text -> Set Text
anagramsFor x = filter (\w -> x `isDistinctOf` w && x `isAnagramOf` w)
  where
    isDistinctOf = (/=) `on` toLower
    isAnagramOf  = (==) `on` fromList . unpack . toLower
