module Proverb(recite) where

import Data.List (intercalate)

type Input = String
type Recital = String

phrase :: Input -> Input -> String
phrase x y = "For want of a " ++ x ++ " the " ++ y ++ " was lost."

ending :: Input -> String
ending w = "And all for the want of a " ++ w ++ "."

joinLines :: [String] -> Recital
joinLines = intercalate "\n"

recite :: [Input] -> Recital
recite [] = ""
recite inputs@(x:xs) =
  let parts = zipWith phrase inputs xs
  in joinLines $ parts ++ [ending x]
