{-# LANGUAGE OverloadedStrings #-}
module PigLatin (translate) where

import qualified Data.Text as T
import           Data.Text (Text)

translate :: Text -> Text
translate = T.unwords . map translateWord . T.words

translateWord :: Text -> Text
translateWord = ay . transform . consonantCluster

ay :: (Text, Text) -> Text
ay (consonants, rest) = rest <> consonants <> "ay"

transform :: (Text, Text) -> (Text, Text)
transform (consonants, rest)
  -- Rule 1: xray -> xrayay, yttria -> yttriaay, queen -> eenquay
  | beginsWith "xr" = (rest, consonants)
  | beginsWith "yt" = (rest, consonants)
  | beginsWithVowels = (consonants, rest)

  -- Rule 3: square -> aresquay
  | containsQu = (consonants <> "u", T.drop 1 rest)

  -- Rule 2: equal -> equalay, rhythm -> ythmrhay, my -> ymay
  | otherwise = (consonants, rest)
  where
    beginsWith = (`T.isPrefixOf` consonants)
    beginsWithVowels = T.null consonants
    containsQu = "q" `T.isSuffixOf` consonants && "u" `T.isPrefixOf` rest

-- When a 'y' occurs beyond the first letter of a word, the consonant
-- cluster ends before it.
consonantCluster :: Text -> (Text, Text)
consonantCluster = y . T.span (not . isVowel)
  where
    y :: (Text, Text) -> (Text, Text)
    y (consonants, rest) = case T.findIndex (== 'y') (T.drop 1 consonants) of
      Nothing -> (consonants, rest)
      Just n  -> let (consonants', middle) = T.splitAt (n + 1) consonants
                 in (consonants', middle <> rest)

-- The letter 'y' is only a vowel conditionally.
isVowel :: Char -> Bool
isVowel c = T.any (== c) "aeiou"
