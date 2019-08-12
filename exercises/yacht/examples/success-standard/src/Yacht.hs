module Yacht (yacht, Category(..)) where

import Data.List

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht _ []   = error "You lose!"
yacht Ones   dice = simpleScore 1 dice
yacht Twos   dice = simpleScore 2 dice
yacht Threes dice = simpleScore 3 dice
yacht Fours  dice = simpleScore 4 dice
yacht Fives  dice = simpleScore 5 dice
yacht Sixes  dice = simpleScore 6 dice

yacht FullHouse dice
  | setSize == 2 && length two == 2 && three' == 3 = sum dice
  | otherwise = 0
  where
    sortedDice  = sortDiceByGroupSize dice
    setSize     = length sortedDice
    (two:three) = sortedDice
    three'      = (length. head') three

yacht FourOfAKind dice
  | length lg == 4 = sum lg
  | length lg == 5 = 4 * head dice
  | otherwise      = 0
  where
    lg = largestGroup dice

yacht LittleStraight dice
  | isSucc dice && maximum dice == 5 = 30
  | otherwise = 0

yacht BigStraight dice
  | isSucc dice && maximum dice == 6 = 30
  | otherwise = 0

yacht Choice dice = sum dice

yacht Yacht (die:dice)
  | all (== die) dice = 50
  | otherwise         = 0

simpleScore :: Int -> [Int] -> Int
simpleScore die = sum . filter (== die)

sortDiceByGroupSize :: ((Ord a), Eq a) => [a] -> [[a]]
sortDiceByGroupSize = sortOn length . group . sort

largestGroup :: ((Ord a), Eq a) => [a] -> [a]
largestGroup = last . sortDiceByGroupSize

isSucc :: [Int] -> Bool
isSucc = all (\(x,y) -> succ x == y) . pairwise . sort
  where pairwise = zip <*> tail

head' :: [[a]] -> [a]
head' [] = []
head' xs = head xs
