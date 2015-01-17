module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n s = map (take n) . take (length s - n + 1) . tails $ ints
    where ints = map digitToInt s