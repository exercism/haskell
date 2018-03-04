{-# LANGUAGE LambdaCase #-}

module Transpose (transpose) where

import Data.Maybe (isNothing)

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
    toString line = concatMap (\case Nothing -> " "
                                     Just c -> c:"") $ dropRight line
    dropRight line = snd $ foldr 
          (\x (ignore, xs) -> if ignore && isNothing x 
                                then (ignore, xs) 
                                else (False, x:xs)) (True,[]) line
