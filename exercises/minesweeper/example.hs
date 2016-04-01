module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.Array (listArray, (!))

annotate :: [String] -> [String]
annotate board = [[ out (x, y) | x <- [1 .. xn]] | y <- [1 .. yn]]
  where
    yn = length board
    -- assume rectangular input
    xn | yn > 0    = length (head board)
       | otherwise = 0
    out ix
      | mines ! ix = '*'
      | otherwise  = showMineCount . sum . neighbors $ ix
    showMineCount 0 = ' '
    showMineCount i = intToDigit i
    isMine = ('*' ==)
    mines = listArray ((1, 1), (xn, yn)) (map isMine (concat board))
    neighbors (x, y) =
      [ fromEnum (mines ! (x', y'))
      | y' <- [max 1 (y - 1) .. min yn (y + 1)]
      , x' <- [max 1 (x - 1) .. min xn (x + 1)]
      , x' /= x || y' /= y
      ]
