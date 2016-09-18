module Connect (Mark(..), resultFor) where

data Mark = Cross | Nought deriving (Eq, Show)

resultFor :: [String] -> Maybe Mark
resultFor = undefined
