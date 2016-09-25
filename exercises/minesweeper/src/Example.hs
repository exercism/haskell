module Minesweeper (annotate) where

import Data.Char  (intToDigit)
import Data.Maybe (mapMaybe)

annotate :: [String] -> [String]
annotate xss = [[ fixTile x (i, j)
                | (j, x ) <- zip [0..] xs  ]
                | (i, xs) <- zip [0..] xss ]
  where

    bombs = length . filter (== '*') . mapMaybe (`lookup` tiles) . neighbors

    fixTile ' ' position = case bombs position of
        0 -> ' '
        x -> intToDigit x
    fixTile x _ = x

    neighbors (i, j) = [ (i - 1, j - 1), (i - 1, j), (i - 1, j + 1)
                       , (i    , j - 1),             (i    , j + 1)
                       , (i + 1, j - 1), (i + 1, j), (i + 1, j + 1) ]

    tiles = [ ((i, j), x)
            | (i, xs) <- zip [0 :: Int ..] xss
            , (j, x ) <- zip [0 :: Int ..] xs  ]
