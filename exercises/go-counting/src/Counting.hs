module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- | Returns the coordinates (1 based, top left is (1,1)) of of the points
-- in each territory along with who "owns" the territory. A territory is
-- owned by one of the players if that player's stones are the only
-- stones adjacent to the territory.
territories :: [[Char]] -> [(Set Coord, Maybe Color)]
territories = undefined

-- | Returns the territory that contains the coordinate along with the
-- owner of the territory. If the coordinate does not point to an empty
-- location returns Nothing.
territoryFor :: [[Char]] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor = undefined
