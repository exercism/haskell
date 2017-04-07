module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = concatMap initials . words . map replaceNonAlpha

initials :: String -> String
initials [] = []
initials (x:xs) = toUpper x : rest
  where rest = if not $ isAcronym xs
               then filter isUpper xs
               else []

isAcronym :: String -> Bool
isAcronym = all isUpper

replaceNonAlpha :: Char -> Char
replaceNonAlpha x
  | isAlpha x = x
  | otherwise = ' '
