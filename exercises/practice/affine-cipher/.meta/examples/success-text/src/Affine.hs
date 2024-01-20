module Affine (decode, encode) where

import           Data.Char
import           Data.List (find)
import qualified Data.Text as T
import           Data.Text (Text)

alphabetStart :: Int
alphabetStart = ord 'a'

alphabetLength :: Int
alphabetLength = 26

encode :: (Int, Int) -> Text -> Maybe Text
encode (keyA, keyB) cipherText = processText <$> findMMI keyA
  where
    processText mmi = T.unwords $ T.chunksOf 5 $ translateString (func mmi) cipherText
    func _ idx = (keyA*idx + keyB) `mod` alphabetLength

decode :: (Int, Int) -> Text -> Maybe Text
decode (keyA, keyB) plainText = processText <$> findMMI keyA
  where
    processText mmi = translateString (func mmi) plainText
    func mmi idx = mmi*(idx-keyB) `mod` alphabetLength

translateString :: (Int -> Int) -> Text -> Text
translateString func text = T.map translateChar $ T.filter isAlphaNum text 
  where
    translateChar ch = if isDigit ch then ch else fromIndex $ func $ toIndex $ toLower ch
    fromIndex idx = chr (idx + alphabetStart)
    toIndex ch = ord ch - alphabetStart

findMMI :: Int -> Maybe Int
findMMI keyA = find isMMI [1..alphabetLength]
  where
    isMMI x = keyA*x `mod` alphabetLength == 1
