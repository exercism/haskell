{-# LANGUAGE OverloadedStrings #-}
module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLower)
import qualified Data.Text as T
import           Data.Text (Text)

responseFor :: Text -> Text
responseFor s
  | isSilent s = "Fine. Be that way!"
  | isYelling s && isAsking s = "Calm down, I know what I'm doing!"
  | isYelling s = "Whoa, chill out!"
  | isAsking s = "Sure."
  | otherwise = "Whatever."

isSilent, isYelling, isAsking :: Text -> Bool
isSilent = T.all isSpace
isYelling = (&&) <$> T.any isUpper <*> T.all (not . isLower)
isAsking = (Just '?' ==) . fmap snd . T.unsnoc . T.stripEnd
