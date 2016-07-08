module POV (Graph(Graph), fromPOV, tracePathBetween) where

data Graph a = Graph a [Graph a] deriving (Eq, Show)

fromPOV :: Eq a => a -> Graph a -> Maybe (Graph a)
fromPOV = undefined

tracePathBetween :: Eq a => a -> a -> Graph a -> Maybe [a]
tracePathBetween = undefined
