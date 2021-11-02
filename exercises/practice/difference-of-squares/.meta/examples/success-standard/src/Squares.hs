module Squares (squareOfSum, sumOfSquares, difference) where

square :: Integral a => a -> a
square n = n * n

squareOfSum :: Integral a => a -> a
squareOfSum n = square $ n * succ n `div` 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (2 * n3 + 3 * n2 + n) `div` 6
  where n2 = square n
        n3 = n * n2

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n
