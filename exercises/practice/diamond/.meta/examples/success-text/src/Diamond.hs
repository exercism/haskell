module Diamond (diamond) where

import Data.Char (ord, chr)
import Data.Text (Text, pack)

-- Very low-effort, since this is just the String solution with an extra pack.
-- This is mostly here to validate that the tests work with Char -> Maybe [Text],
-- rather than show off any use of Data.Text.
diamond :: Char -> Maybe [Text]
diamond c | c `notElem` ['A'..'Z'] = Nothing
diamond c = Just . mirror . map row $ [0..n] where
  n = ord c - ord 'A'
  mirror top = top ++ tail (reverse top)
  row i = pack . mirror $ spaces (n - i) ++ [letter i] ++ spaces i
  letter i = chr (ord 'A' + i)
  spaces i = replicate i ' '
