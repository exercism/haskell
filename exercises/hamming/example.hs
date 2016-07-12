module Hamming (distance) where

distance :: Integral a => String -> String -> Maybe a
distance (x:xs) (y:ys) = fmap (if x /= y then (+1) else id) $ distance xs ys
distance    []     []  = Just 0
distance     _      _  = Nothing
