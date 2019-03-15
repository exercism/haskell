module Isogram (isIsogram) where

import Data.Char (isLetter)
import Data.Maybe (isJust)
import Data.MonoTraversable (ofoldM)
import Data.Set (Set, empty, member, insert)
import Data.Text (Text)
import qualified Data.Text as T

isIsogram :: Text -> Bool
isIsogram = isJust . ofoldM gather empty . T.toLower . T.filter isLetter
  where
    gather :: Set Char -> Char -> Maybe (Set Char)
    gather seen c
      | c `member` seen = Nothing
      | otherwise = return (insert c seen)
