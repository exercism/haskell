module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board = error "You need to implement this function."
