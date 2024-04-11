module GameOfLife (tick) where

import Data.Maybe (mapMaybe)

tick :: [[Int]] -> [[Int]]
tick matrix = [[ nextGen x (i, j)
               | (j, x ) <- zip [0..] xs  ]
               | (i, xs) <- zip [0..] matrix ]
    where
        living = length . filter (== 1) . mapMaybe (`lookup` cells) . neighbors

        neighbors (i, j) = [ (i - 1, j - 1), (i - 1, j), (i - 1, j + 1)
                           , (i    , j - 1),             (i    , j + 1)
                           , (i + 1, j - 1), (i + 1, j), (i + 1, j + 1) ]

        nextGen cell position =
            let around = living position in
            if (cell == 1 && (around == 2 || around == 3)) || (cell == 0 && around == 3) then 1 else 0

        cells = [ ((i, j), x)
                | (i, xs) <- zip [0 :: Int ..] matrix
                , (j, x ) <- zip [0 :: Int ..] xs  ]
