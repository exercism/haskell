module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories = undefined

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor = undefined
