{-# LANGUAGE TypeFamilies #-}

module WordCount (wordCount) where

import Prelude hiding (head, init, null, tail)
import Data.Char      (isAlphaNum)
import Data.List      (foldl')
import Data.Text      (Text, head, init, null, split, tail, toLower)

import qualified Data.Map as Map

wordCount :: Text -> Map.Map Text Int
wordCount = foldl' addOne Map.empty
          . map (stripQuote . toLower)
          . wordsBy (\c -> not (isAlphaNum c) && c /= '\'')

addOne :: Map.Map Text Int -> Text -> Map.Map Text Int
addOne m word = Map.insertWith (+) word 1 m

-- The `text` package misses this function that
-- exists in package `split`, but works on lists.
wordsBy :: (Char -> Bool) -> Text -> [Text]
wordsBy p = filter (not . null) . split p

stripQuote :: Text -> Text
stripQuote t = if head t == '\'' then init (tail t) else t
