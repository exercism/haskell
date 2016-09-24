module Raindrops (convert) where

import Data.Maybe (fromMaybe)

convert :: Int -> String
convert n = fromMaybe (show n) maybeSound
  where
    maybeSound = sound "Pling" 3 `mappend`
                 sound "Plang" 5 `mappend`
                 sound "Plong" 7
    sound noise factor | n `rem` factor == 0 = Just noise
                       | otherwise           = Nothing
