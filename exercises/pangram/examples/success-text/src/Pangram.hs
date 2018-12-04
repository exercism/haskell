module Pangram (isPangram) where

import           Data.Text (Text)
import qualified Data.Text as T

isPangram :: Text -> Bool
isPangram s = all (`member` T.toLower s) ['a'..'z']
  where member = T.any . (==)
