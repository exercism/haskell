module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square x
    | x < 1     = Nothing
    | x > 64    = Nothing
    | otherwise = Just . (2^) . pred $ x

total :: Integer
total = sum . map (fromJust . square) $ [1..64]
