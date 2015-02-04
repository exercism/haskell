module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = f x : accumulate f xs

{-

-- Some other reasonable definitions:

accumulate f = foldr ((:) . f) []

accumulate f xs = [f x | x <- xs]

-- Commonly submitted inefficient solution (we test for this now):

accumulate f xs = accumulate' []
  where
    accumulate' acc []     = acc
    accumulate' acc (x:xs) = accumulate' (f x : acc) xs

-}
