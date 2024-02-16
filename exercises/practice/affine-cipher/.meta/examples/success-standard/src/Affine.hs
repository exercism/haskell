module Affine (decode, encode) where

import Data.Char (chr, isAlphaNum, isDigit, ord, toLower)
import Data.List (find)
import Data.List.Split (chunksOf)

alphabetStart :: Int
alphabetStart = ord 'a'

alphabetLength :: Int
alphabetLength = 26

encode :: (Int, Int) -> String -> Maybe String
encode (keyA, keyB) cipherText = processText <$> findMMI keyA
  where
    processText mmi = unwords $ chunksOf 5 $ translateString (func mmi) cipherText
    func _ idx = (keyA*idx + keyB) `mod` alphabetLength

decode :: (Int, Int) -> String -> Maybe String
decode (keyA, keyB) plainText = processText <$> findMMI keyA
  where
    processText mmi = translateString (func mmi) plainText
    func mmi idx = mmi*(idx-keyB) `mod` alphabetLength

translateString :: (Int -> Int) -> String -> String
translateString func text = map translateChar $ filter isAlphaNum text 
  where
    translateChar ch = if isDigit ch then ch else fromIndex $ func $ toIndex $ toLower ch
    fromIndex idx = chr (idx + alphabetStart)
    toIndex ch = ord ch - alphabetStart

findMMI :: Int -> Maybe Int
findMMI keyA = find isMMI [1..alphabetLength]
  where
    isMMI x = keyA*x `mod` alphabetLength == 1
