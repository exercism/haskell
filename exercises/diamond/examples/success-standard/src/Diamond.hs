module Diamond (diamond) where

import Data.Char (ord, chr)

-- replicate blank
rb :: Int -> String
rb x = replicate x ' '

helper :: (Char, (Int, Int)) -> String
helper (c, (0, y)) = rb y ++ [c] ++ rb y
helper (c, (x, y)) = rb y ++ [c] ++ rb x ++ [c] ++ rb y

diamond :: Char -> [String]
diamond = (\x -> x ++ tail (reverse x)) . f . subtract 64 . ord
  where rows x = zip (0 : take (x-1) [1, 3..]) [x-1, x-2..0]
        f      = zipWith (curry helper) abc . rows
        abc    = map chr [65..90]
