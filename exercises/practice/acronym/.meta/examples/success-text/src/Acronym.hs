{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import           Data.Char (isUpper, isLower, isLetter, toUpper)
import qualified Data.Text as T
import           Data.Text (Text)

abbreviate :: Text -> Text
abbreviate s = T.filter (/= ' ') $ T.zipWith sanitize (" " <> s) (s <> " ")

sanitize :: Char -> Char -> Char
sanitize a b
  | not (isLetter a) && a /= '\'' && isLetter b = toUpper b
  | isLower a && isUpper b = b
  | otherwise = ' '
