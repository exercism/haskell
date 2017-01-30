module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f = accumulate' []
  where
    accumulate' acc []     = reverse acc
    accumulate' acc (x:xs) = accumulate' (f x : acc) xs
