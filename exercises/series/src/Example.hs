module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices 0 _ = [[]]
slices n s = map (take n) . take (length s - n + 1) . tails $ numberSeries
    where numberSeries = map digitToInt s