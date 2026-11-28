module TreeBuilding (Record(..), Tree, buildTree, node) where

import qualified Data.IntMap.Strict as IntMap
import           Data.List          (sortOn)
import qualified Data.Tree          as T

data Record = Record { recordId :: Int, parentId :: Int } deriving (Eq, Show)

type Tree = T.Tree Int

node :: Int -> [Tree] -> Tree
node = T.Node

buildTree :: [Record] -> Maybe Tree
buildTree records
  | null records                              = Nothing
  | map recordId sorted /= [0 .. n - 1]       = Nothing
  | not (all validParent sorted)              = Nothing
  | otherwise                                 = Just (T.unfoldTree expand 0)
  where
    n = length records
    sorted = sortOn recordId records

    validParent (Record 0 p) = p == 0
    validParent (Record i p) = p < i

    -- Folding from the right keeps each list of children in ascending order.
    children = foldr addChild IntMap.empty (drop 1 sorted)
    addChild (Record i p) = IntMap.insertWith (++) p [i]

    expand i = (i, IntMap.findWithDefault [] i children)
