module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n =
  [ (a, b, c)
  | a <- [1 .. n `div` 3]
  , let b = n `div` 2 - a * n `div` (2 * (n - a))
  , let c = n - a - b
  , a < b
  , a * a + b * b == c * c
  ]
