module Diamond (diamond) where

import Data.Char (ord, chr)

pad :: Int -> String
pad x = replicate x ' '

oneRow :: Char -> (Int, Int) -> String
oneRow c (0, y) = pad y ++ [c] ++ pad y
oneRow c (x, y) = pad y ++ [c] ++ pad x ++ [c] ++ pad y

diamond :: Char -> Maybe [String]
diamond = Just . (\x -> x ++ tail (reverse x)) . mkTop . subtract 64 . ord
  where rows x = zip (0 : take (x-1) [1, 3..]) [x-1, x-2..0]
        mkTop  = zipWith oneRow abc . rows
        abc    = map chr [65..90]
