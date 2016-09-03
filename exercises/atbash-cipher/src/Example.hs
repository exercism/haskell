module Atbash (decode, encode) where

import Data.Char       (isAsciiLower, isDigit, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe      (mapMaybe)

decode :: String -> String
decode = mapMaybe atbashChar

encode :: String -> String
encode = unwords . chunksOf 5 . mapMaybe atbashChar

atbashChar :: Char -> Maybe Char
atbashChar c
    | isDigit c       = Just c
    | isAsciiLower lc = Just symmetric
    | otherwise       = Nothing
  where
    lc = toLower c
    symmetric = toEnum $ fromEnum 'a'
                       + fromEnum 'z'
                       - fromEnum lc
