module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV = error "You need to implement this function."

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween = error "You need to implement this function."
