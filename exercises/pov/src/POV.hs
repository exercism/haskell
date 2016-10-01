module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV = undefined

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween = undefined
