module Dominoes (chain) where

import Data.List  (delete)
import Data.Maybe (listToMaybe, mapMaybe)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain (x@(left, right):xs) = fmap (x:) (subchain xs left right)

subchain :: [(Int, Int)] -> Int -> Int -> Maybe [(Int, Int)]
subchain [] chainLeft chainRight = if chainLeft == chainRight then Just [] else Nothing
subchain l  chainLeft chainRight = listToMaybe (mapMaybe subchain' l)
  where subchain' (a, b) | a == chainRight = fmap ((a, b):) (subchain (delete (a, b) l) chainLeft b)
        subchain' (a, b) | b == chainRight = fmap ((b, a):) (subchain (delete (a, b) l) chainLeft a)
        subchain' _                        = Nothing
