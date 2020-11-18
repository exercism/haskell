module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = undefined
accumulate f (x:xs) = f x : accumulate f xs

{-

-- Some other reasonable definitions:

accumulate f = foldr ((:) . f) []

accumulate f xs = [f x | x <- xs]

-}
