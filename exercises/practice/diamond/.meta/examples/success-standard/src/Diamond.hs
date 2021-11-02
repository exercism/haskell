module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c | c `notElem` ['A'..'Z'] = Nothing
diamond c = Just . mirror . map row $ [0..n] where
  n = ord c - ord 'A'
  mirror top = top ++ tail (reverse top)
  row i = mirror $ spaces (n - i) ++ [letter i] ++ spaces i
  letter i = chr (ord 'A' + i)
  spaces i = replicate i ' '
