module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [[Char]] -> [(Set Coord, Maybe Color)]
territories = undefined

territoryFor :: [[Char]] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor = undefined
