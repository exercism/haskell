module Connect (resultFor, Color(..)) where

data Color = Black
           | White
           deriving (Show,Eq)

resultFor :: [String] -> Maybe Color
resultFor = undefined
