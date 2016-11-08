{-# LANGUAGE TypeFamilies #-}

module WordCount (wordCount) where

import Prelude hiding (head, init, null, tail)
import Data.Char      (isAlphaNum)
import Data.Text      (Text, head, init, null, split, tail, toLower)
import Data.MultiSet  (MultiSet, Occur, fromList, fromOccurList, toOccurList)

import qualified GHC.Exts (IsList(..))

wordCount :: Text -> Bag Text
wordCount = Bag
          . fromList
          . map (stripQuote . toLower)
          . wordsBy (\c -> not (isAlphaNum c) && c /= '\'')

-- The `text` package misses this function that
-- exists in package `split`, but works on lists.
wordsBy :: (Char -> Bool) -> Text -> [Text]
wordsBy p = filter (not . null) . split p

stripQuote :: Text -> Text
stripQuote t = if head t == '\'' then init (tail t) else t

-- MultiSet is not an instance of `IsList`, so we create
-- a newtype to wrap it, avoiding an orphan instance.
newtype Bag a = Bag { toMultiSet :: MultiSet a }

instance (Ord a) => GHC.Exts.IsList (Bag a)
  where
    type Item (Bag a) = (a, Occur)
    fromList = Bag . fromOccurList
    toList   = toOccurList . toMultiSet
