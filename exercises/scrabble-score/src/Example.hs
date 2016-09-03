module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import Data.Map  (findWithDefault, fromList)

scoreLetter :: Num a => Char -> a
scoreLetter letter = findWithDefault 0 (toUpper letter) scoreMap
  where
    scoreMap   = fromList [(k,v) | (ks, v) <- scoreTable, k <- ks]
    scoreTable = [ ("AEIOULNRST",  1)
                 , ("DG"        ,  2)
                 , ("BCMP"      ,  3)
                 , ("FHVWY"     ,  4)
                 , ("K"         ,  5)
                 , ("JX"        ,  8)
                 , ("QZ"        , 10) ]

scoreWord :: Num a => String -> a
scoreWord = sum . map scoreLetter
