module RotationalCipher (rotate) where

import Data.List (elemIndex)

rotate :: Int -> String -> String
rotate r = map (f r ['a'..'z'] . f r ['A'..'Z'])
  where f x abc e = case elemIndex e abc of
                      Nothing -> e
                      Just a  -> abc!!mod (a+x) 26
