module Yacht (yacht, Category(..)) where

import Control.Monad (join)
import Data.List (sortOn, sort, group)

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
yacht Ones   = valueScore 1
yacht Twos   = valueScore 2
yacht Threes = valueScore 3
yacht Fours  = valueScore 4
yacht Fives  = valueScore 5
yacht Sixes  = valueScore 6

yacht FullHouse = sum
                . join
                . (\xs -> if length xs == 2 then xs else [])
                . filter ((<= 3) . length)
                . diceGroups

yacht FourOfAKind = sum
                  . take 4
                  . join
                  . filter ((>= 4) . length)
                  . diceGroups

yacht LittleStraight = verify . sort
  where verify [1, 2, 3, 4, 5] = 30
        verify _ = 0

yacht BigStraight = verify . sort
  where verify [2, 3, 4, 5, 6] = 30
        verify _ = 0

yacht Choice = sum

yacht Yacht = (* 10)
            . length
            . join
            . filter((== 5) . length)
            . diceGroups

valueScore :: Int -> [Int] -> Int
valueScore = (sum .) . filter . (==)

diceGroups :: ((Ord a), Eq a) => [a] -> [[a]]
diceGroups = sortOn length . group . sort
