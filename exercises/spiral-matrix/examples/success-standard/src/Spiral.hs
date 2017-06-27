module Spiral (spiral) where

-- | Create a square matrix of natural numbers in a inward, clockwise spiral.
spiral :: Int -> [[Int]]
spiral l = matrix (spiralIndex (l, l)) (l, l)
  where
    -- If an index isn't in the first row of a matrix, it is in the
    -- 90 degress-rotated, complementary matrix.
    spiralIndex :: Integral a => (a, a) -> (a, a) -> a
    spiralIndex     _  (1, j) = j
    spiralIndex (h, w) (i, j) = w + spiralIndex (w, h - 1) (w - j + 1, i - 1)

-- Build a lazy, list-based matrix based on a formula.
matrix :: ((Int, Int) -> a) -> (Int, Int) -> [[a]]
matrix f (rows, columns) = [[ f (i, j)
                            | j <- [1..columns] ]
                            | i <- [1..rows   ] ]
