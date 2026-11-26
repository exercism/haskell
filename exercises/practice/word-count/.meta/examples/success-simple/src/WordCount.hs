module WordCount (wordCount) where

import Control.Arrow   ((&&&))
import Data.Char       (toLower, isAlphaNum)
import Data.List       (dropWhileEnd, group, sort)
import Data.List.Split (wordsBy)

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length)
          . group
          . sort
          . filter (not . null)
          . map (stripQuote . map toLower)
          . wordsBy (\c -> not (isAlphaNum c) && c /= '\'')

stripQuote :: String -> String
stripQuote = dropWhileEnd (== '\'') . dropWhile (== '\'')
