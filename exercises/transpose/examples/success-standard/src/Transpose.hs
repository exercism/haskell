module Transpose (transpose) where

import Data.Maybe (fromMaybe, isNothing)

transpose :: [String] -> [String]
transpose [] = []
transpose lines =
  map toString $ foldr 
    (zipWith (:) . padRight . map Just) 
    (replicate longestLength []) 
    lines
  where 
    longestLength = maximum $ map length lines
    padRight line = line ++ replicate (longestLength - length line) Nothing
    toString line = concatMap (\x -> case x of Nothing -> " "
                                               Just c -> c:"") $ dropRight line
    dropRight line = snd $ foldr 
          (\x (ignore, xs) -> if ignore && isNothing x 
                                then (ignore, xs) 
                                else (False, x:xs)) (True,[]) line
