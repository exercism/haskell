module WordCount (wordCount) where

import Control.Arrow   ((&&&))
import Data.Char       (toLower, isAlphaNum)
import Data.List       (group, sort)
import Data.List.Split (wordsBy)

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length)
          . group
          . sort
          . map (map toLower)
          . wordsBy (not . isAlphaNum)
