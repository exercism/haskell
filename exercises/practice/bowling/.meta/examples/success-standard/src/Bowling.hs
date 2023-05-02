module Bowling (score, BowlingError(..)) where

import Data.List  (tails)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

type Roll = Int

data Frame = Frame { whichFrame :: Int, rolls :: [Roll] }

type ScoreState = Either BowlingError (Frame, Int, Int) -- (frame, rollIndex, totalScore)

score :: [Int] -> Either BowlingError Int
score rolls' = totalScoreState >>= completedScore
  where
    totalScoreState = foldl scoreRoll startState rolls''
    startState = Right (Frame 1 [], 0, 0)
    rolls'' = filter (not . null) $ tails rolls'
    completedScore :: (Frame, Int, Int) -> Either BowlingError Int
    completedScore (f, _, totalScore)
      | complete f && finalFrame f = Right totalScore
      | otherwise = Left IncompleteGame

scoreRoll :: ScoreState -> [Roll] -> ScoreState
scoreRoll scoreState rolls' = do
  (f, ri, s) <- scoreState
  scoreRoll' f ri s rolls'

scoreRoll' :: Frame -> Int -> Int -> [Roll] -> ScoreState
scoreRoll' _ ri _ (r:_) | r < 0 || r > 10 = Left $ InvalidRoll ri r
scoreRoll' f@(Frame 10 (fr:_)) ri totalScore (r:_) = case rolls f of
  [_] -> Right (addRoll f r, succ ri, totalScore + fr + r)
  [firstRoll, secondRoll] | fr == 10 || spare f -> addFillBall firstRoll secondRoll
                          | otherwise -> noMoreRollsPossible
  _ -> noMoreRollsPossible
  where
    noMoreRollsPossible = Left $ InvalidRoll ri r
    addFillBall :: Int -> Int -> ScoreState
    addFillBall firstRoll secondRoll
      | firstRoll == 10 && secondRoll /= 10 && secondRoll + r > 10 = Left $ InvalidRoll ri r
      | otherwise = Right (addRoll f r, succ ri, totalScore + r)
scoreRoll' f@(Frame n _) ri totalScore rs@(r:_)
  | complete f = Right (Frame (succ n) [r], succ ri, totalScore + fscore f rs)
  | pins f + r <= 10 = Right (addRoll f r, succ ri, totalScore)
scoreRoll' _ ri _ (r:_) = Left $ InvalidRoll ri r
scoreRoll' _ _ _ _ = undefined -- to avoid compiler warning

-- Frame functions
addRoll :: Frame -> Roll -> Frame
addRoll f r = Frame (whichFrame f) (rolls f ++ [r])

throws :: Frame -> Int
throws = length . rolls

pins :: Frame -> Int
pins = sum . rolls

strike :: Frame -> Bool
strike f = throws f == 1 && pins f == 10

spare :: Frame -> Bool
spare f = throws f == 2 && pins f == 10

finalFrame :: Frame -> Bool
finalFrame (Frame n _) = n == 10

complete :: Frame -> Bool
complete f
  | not $ finalFrame f = strike f || throws f == 2
  | head (rolls f) == 10 || sum (take 2 $ rolls f) == 10 = throws f == 3
  | otherwise = throws f == 2

fscore :: Frame -> [Roll] -> Int
fscore f rs = pins f + bonus
  where
    bonus
      | finalFrame f = 0
      | strike f = nextRoll + nextToNextRoll
      | spare f  = nextRoll
      | otherwise = 0
    nextRoll = head rs
    nextToNextRoll = head $ tail rs

