{-# LANGUAGE OverloadedStrings #-}
module Isogram (isIsogram) where

import qualified Data.Char as C
import qualified Data.Maybe as M
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text (Text)

import Data.MonoTraversable (ofoldM)

isIsogram :: Text -> Bool
isIsogram = M.isJust . isogram

isogram :: Text -> Maybe (Set Char)
isogram = ofoldM gather S.empty . T.toLower
  where
    gather :: Set Char -> Char -> Maybe (Set Char)
    gather seen c
      | c `S.member` seen = Nothing
      | not (C.isLetter c) = return seen
      | otherwise = return (S.insert c seen)
