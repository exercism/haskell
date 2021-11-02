 {-# LANGUAGE OverloadedStrings #-}
module Raindrops (convert) where

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text (Text)

convert :: Int -> Text
convert n = fromMaybe (T.pack $ show n) $
  sound "Pling" 3 <>
  sound "Plang" 5 <>
  sound "Plong" 7
  where
    sound noise factor
      | n `rem` factor == 0 = Just noise
      | otherwise           = Nothing
