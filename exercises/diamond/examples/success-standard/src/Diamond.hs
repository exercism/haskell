module Diamond (diamond) where

import Data.Char (ord, chr)

diamondH :: (Int -> String) -> (Char, (Int, Int)) -> String
diamondH rb (c, (0, y)) = rb y ++ [c] ++ rb y
diamondH rb (c, (x, y)) = rb y ++ [c] ++ rb x ++ [c] ++ rb y

diamond :: Char -> [String]
diamond = (\x -> x ++ tail (reverse x)) . f . abs . (64-) . ord
  where rows x = zip (0 : take (x-1) [1,3..]) [x-1,x-2..0]
        f      = map (diamondH rb) . zip (map chr [65..90]) . rows
        rb x   = replicate x ' '
