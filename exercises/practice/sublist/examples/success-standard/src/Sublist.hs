module Sublist (sublist) where
import Data.List (isInfixOf)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys          = Just EQ
  | xs `isInfixOf` ys = Just LT
  | ys `isInfixOf` xs = Just GT
  | otherwise         = Nothing
