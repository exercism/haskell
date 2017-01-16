module Sublist (Sublist(..), sublist) where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist :: [a] -> [a] -> Sublist
sublist = error "You need to implement this function."
