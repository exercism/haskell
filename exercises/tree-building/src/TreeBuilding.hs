module TreeBuilding (newTree, Record(..), Tree(..)) where

import Data.List
import Data.Ord (comparing)
--import Data.Maybe (fromMaybe)

type Id = Int
type Children = [Tree]

data Record = Record Id (Maybe Id) deriving (Show)

data Tree = Leaf Id | Branch Id Children deriving (Eq, Show)

newTree :: [Record] -> Maybe Tree
newTree records
  | cycleCheck records = Nothing
  | succIdCheck records == True = build . groupByParent $ records
  | otherwise = Nothing
  where
    recordParent = \(Record _ p) -> p
    recordId = \(Record i _) -> i
    succIdCheck = all (\(x,y) -> succ x == y) . pairwise . sort . map recordId
      where pairwise = zip <*> tail
    cycleCheck = any id . map (\(r) -> case r of
                                         (Record i Nothing)   -> if i == 0
                                                                 then False
                                                                 else True
                                         (Record i (Just p)) -> if p >= i
                                                                 then True
                                                                 else False)
    groupByParent = sortOn fst . map (\xs -> (recordParent (head xs), map recordId xs)) . groupBy parentsEq . sortBy (comparing recordParent) . sortBy (comparing recordId)
    parentsEq rx ry = recordParent rx == recordParent ry
    build [] = Nothing
    build (x:xs)
      | not . rootCheck $ x = Nothing
      | xs == []    = Just (Leaf 0)
      | otherwise   = Just (Branch 0 (recursiveBuilding xs))
      where
        rootCheck (p,ids)
          | p /= Nothing || (length ids /= 1 && (head ids) /= 0) = False
          | otherwise = True
        recursiveBuilding ys
          | length ys == 1 = map (\yid -> Leaf yid) (snd (head ys))
          | otherwise      = (\(z:zs) -> map (\xid ->
                                                let children = filter ((== (Just xid)) . fst) zs
                                                in  if length children > 0
                                                    then Branch xid (recursiveBuilding children)
                                                    else Leaf xid
                                             ) (snd z)) ys
