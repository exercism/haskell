module RotationalCipher (rotate) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isAsciiUpper, isAsciiLower, chr, ord)

rotate :: Int -> Text -> Text
rotate = T.map . rotateLetter

rotateLetter :: Int -> Char -> Char
rotateLetter amountToRotate char
  | isAsciiLower char = rotatedLetterStartingWith 'a'
  | isAsciiUpper char = rotatedLetterStartingWith 'A'
  | otherwise = char
  where
    rotatedLetterStartingWith :: Char -> Char
    rotatedLetterStartingWith baseCharacter =
      chr $ (ord char - ord baseCharacter + amountToRotate) `mod` 26 + ord baseCharacter
