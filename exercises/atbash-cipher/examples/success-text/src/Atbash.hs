module Atbash (decode, encode) where

import           Data.Char (isLetter, toLower, isAlphaNum)
import qualified Data.Text as T
import           Data.Text (Text)

decode :: Text -> Text
decode = T.map atbashChar . T.filter isAlphaNum

encode :: Text -> Text
encode = T.unwords . T.chunksOf 5 . decode

atbashChar :: Char -> Char
atbashChar c
    | isLetter c = symmetric
    | otherwise  = c
  where
    lc = toLower c
    symmetric = toEnum $ fromEnum 'a'
                       + fromEnum 'z'
                       - fromEnum lc