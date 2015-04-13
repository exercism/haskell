module POV (Graph(Graph), fromPOV, tracePathBetween) where

import Data.Maybe(listToMaybe, catMaybes)

data Graph a = Graph { value :: a, children :: [Graph a] } deriving (Show, Eq)
data Crumb a = Crumb a [Graph a] [Graph a] deriving (Show, Eq)
data Zipper a = Zipper { node :: (Graph a), path ::  [Crumb a] }

fromPOV :: Eq a => a -> Graph a -> Maybe (Graph a)
fromPOV x = (fmap reparent) . (findLoc x) . rootZipper

tracePathBetween :: Eq a => a -> a -> Graph a -> Maybe [a]
tracePathBetween from to g =
    fromPOV from g |>= rootZipper >>= findLoc to |>= trail |>= map value |>= (++ [to])

reparent :: Eq a => Zipper a -> Graph a
reparent (Zipper g []) = g
reparent (Zipper g (c:cs)) = Graph (value g) $ (children g) ++ [reparented]
    where reparented = reparent (Zipper (crumbToGraph c) cs)

down :: Zipper a -> Maybe (Zipper a)
down (Zipper (Graph v (k:kids)) crumbs) = Just (Zipper k ((Crumb v [] kids):crumbs))
down _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper here ((Crumb v lefts (r:rights)):cs)) = Just (Zipper r (shifted:cs))
    where shifted = Crumb v (lefts ++ [here]) rights
right _ = Nothing

findLoc :: Eq a => a -> Zipper a -> Maybe (Zipper a)
findLoc x loc
    | x == (value . node $ loc) = Just loc
    | otherwise = listToMaybe $ catMaybes $ map look [down, right]
    where look dir = dir loc >>= findLoc x

trail :: Zipper a -> [Graph a]
trail = reverse . (map crumbToGraph) . path

crumbToGraph :: Crumb a -> Graph a
crumbToGraph (Crumb x lefts rights) = Graph x (lefts ++ rights)

rootZipper :: Graph a -> Zipper a
rootZipper g = Zipper g []

infixl 1 |>= -- allow pure and monadic functions in pipe.
(|>=) :: Functor f => f a -> (a -> b) -> f b
(|>=) = flip fmap
