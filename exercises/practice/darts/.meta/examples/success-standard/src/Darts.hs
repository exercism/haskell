module Darts (score) where

score :: Float -> Float -> Int
score x y
    | r <= 1**2 = 10
    | r <= 5**2 = 5
    | r <= 10**2 = 1
    | otherwise = 0
    where
        r = x**2+y**2
