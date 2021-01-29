module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode "" = ""
decode s  = replicate n c ++ decode rest
  where
    i = takeWhile isDigit s
    n = if null i then 1 else read i
    c = head $ filter (not . isDigit) s
    rest = drop (1 + length i) s

encode :: String -> String
encode "" = ""
encode s  = i ++ c : encode rest
  where
    c = head s
    l = length $ takeWhile (== c) s
    i = if l == 1 then "" else show l
    rest = drop l s
