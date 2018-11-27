module Pangram (isPangram) where

import qualified Data.Set as S
import           Data.Set (Set)

import qualified Data.Text as T
import           Data.Text (Text)

isPangram :: Text -> Bool
isPangram = S.null . T.foldl' (flip S.delete) alphabet . T.toLower
  where
    alphabet :: Set Char
    alphabet = S.fromList ['a'..'z']
