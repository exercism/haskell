module Transpose (transpose) where

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isNothing)

transpose :: [String] -> [String]
transpose [] = []
transpose rows =
  map toString $ foldr
    (zipWith (:) . padRight . map Just)
    (replicate longestLength [])
    rows
  where
    longestLength = maximum $ map length rows
    padRight line = line ++ replicate (longestLength - length line) Nothing
    toString line = map (fromMaybe ' ') $ dropWhileEnd isNothing line
