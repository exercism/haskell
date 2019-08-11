module Yacht (yacht, Category(..)) where

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
yacht c []   = error "You lose!"
yacht Ones   dice = simpleScore 1 dice
yacht Twos   dice = simpleScore 2 dice
yacht Threes dice = simpleScore 3 dice
yacht Fours  dice = simpleScore 4 dice
yacht Fives  dice = simpleScore 5 dice
yacht Sixes  dice = simpleScore 6 dice

yacht FullHouse dice
  | (length . uniq) dice == 2 = sum dice
  | otherwise = 0

yacht FourOfAKind dice
  | four == 4 = sum four
  | otherwise = 0
  where four = (length . filter (`elem` dice)) dice

yacht LittleStraight dice
  | isOrdered dice && max dice == 5 = 30
  | otherwise = 0

yacht BigStraight dice
  | isOrdered dice && max dice == 6 = 30
  | otherwise = 0

yacht Choice dice = sum dice

yacht Yacht (d:ice)
  | all (== d) ice = 50
  | otherwise      = 0



simpleScore :: Int -> [Int] -> Int
simpleScore x dice = (length . filter (== x)) dice

uniq :: Eq a => [a] -> [a]
uniq (x:xs) = [ x | x <- (x:xs), not( x `elem` xs) ]

isOrdered :: [Int] -> Bool
isOrdered = all (\(x,y) -> succ x == y) . pairwise
  where pairwise = zip <*> tail
