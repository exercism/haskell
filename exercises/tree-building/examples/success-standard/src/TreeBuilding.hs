module TreeBuilding (newTree, Record(..), Tree(..)) where

import Data.List
import Data.Ord (comparing)

type Id = Int
type Children = [Tree]
type ParentGrouping = (Maybe Id, [Id])

data Record = Record Id (Maybe Id) deriving (Eq, Show)

data Tree = Leaf Id | Branch Id Children deriving (Eq, Show)

newTree :: [Record] -> Maybe Tree
newTree records
  | cycles records = Nothing
  | succIdCheck records == True = build . groupByParent $ records
  | otherwise = Nothing

-- Checks for cycling in newTree
cycles :: [Record] -> Bool
cycles = any id . map checkRecord
  where checkRecord =
          \r -> case r of
                  (Record i Nothing)  -> if' (i /= 0) True False
                  (Record i (Just p)) -> if' (p >= i) True False

build :: [ParentGrouping] -> Maybe Tree
build [] = Nothing
build (x:xs)
  | not . validRoot $ x = Nothing
  | xs == []    = Just (Leaf 0)
  | otherwise   = Just (Branch 0 (build' xs))

--  Internal build
build' :: [ParentGrouping] -> [Tree]
build' xs
  | length xs == 1 = map (\xid -> Leaf xid) (snd (head xs))
  | otherwise      = buildWithChildren xs
  where
    buildWithChildren =
      \(y:ys) -> map (\yid ->
                        let children = filter ((== (Just yid)) . fst) ys
                        in if length children > 0
                           then (Branch yid (build' children))
                           else Leaf yid
                     ) (snd y)

-- Validate the Root node
validRoot :: ParentGrouping -> Bool
validRoot (p, ids) = p == Nothing && length ids == 1 && sum ids == 0

groupByParent :: [Record] -> [ParentGrouping]
groupByParent = sortOn fst
              . map (\xs -> (recordParent (head xs), map recordId xs))
              . groupBy parentsEq
              . sortBy (comparing recordParent)
              . sortBy (comparing recordId)

parentsEq :: Record -> Record -> Bool
parentsEq rx ry = recordParent rx == recordParent ry

recordParent :: Record -> Maybe Int
recordParent = \(Record _ p) -> p

recordId :: Record -> Id
recordId = \(Record i _) -> i

-- verification
succIdCheck :: [Record] -> Bool
succIdCheck = all (\(x,y) -> succ x == y) . pairwise . sort . map recordId
  where pairwise = zip <*> tail

-- General Tools

if' :: Bool -> a -> a -> a
if' p a b = if p then a else b
