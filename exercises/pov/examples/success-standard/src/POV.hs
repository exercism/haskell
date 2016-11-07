module POV (fromPOV, tracePathBetween) where

import Data.Maybe(listToMaybe, mapMaybe)
import Data.Tree(Tree(Node), rootLabel, subForest)

data Crumb a = Crumb a [Tree a] [Tree a] deriving (Show, Eq)
data Zipper a = Zipper { node :: Tree a, path :: [Crumb a] }

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x = fmap reparent . findLoc x . rootZipper

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to g =
    fromPOV from g |>= rootZipper >>= findLoc to |>= trail |>= map rootLabel |>= (++ [to])

reparent :: Eq a => Zipper a -> Tree a
reparent (Zipper g []) = g
reparent (Zipper g (c:cs)) = Node (rootLabel g) $ subForest g ++ [reparented]
    where reparented = reparent (Zipper (crumbToTree c) cs)

down :: Zipper a -> Maybe (Zipper a)
down (Zipper (Node v (k : kids)) crumbs) = Just $ Zipper k $ Crumb v [] kids : crumbs
down _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper here (Crumb v lefts (r : rights) : cs)) = Just $ Zipper r $ shifted : cs
    where shifted = Crumb v (lefts ++ [here]) rights
right _ = Nothing

findLoc :: Eq a => a -> Zipper a -> Maybe (Zipper a)
findLoc x loc
    | x == (rootLabel . node $ loc) = Just loc
    | otherwise = listToMaybe $ mapMaybe look [down, right]
    where look dir = dir loc >>= findLoc x

trail :: Zipper a -> [Tree a]
trail = reverse . map crumbToTree . path

crumbToTree :: Crumb a -> Tree a
crumbToTree (Crumb x lefts rights) = Node x (lefts ++ rights)

rootZipper :: Tree a -> Zipper a
rootZipper g = Zipper g []

infixl 1 |>= -- allow pure and monadic functions in pipe.
(|>=) :: Functor f => f a -> (a -> b) -> f b
(|>=) = flip fmap
