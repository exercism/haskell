module RotationalCipher (rotate) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (elemIndex)

rotate :: Int -> Text -> Text
rotate n = T.map (rotate' n ['a'..'z'] . rotate' n ['A'..'Z'])
  where rotate' amountToRotate letters char =
          case elemIndex char letters of
            Nothing -> char
            Just x  -> letters !! mod (x + amountToRotate) 26
