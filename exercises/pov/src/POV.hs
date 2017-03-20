module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = error "You need to implement this function."

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = error "You need to implement this function."
