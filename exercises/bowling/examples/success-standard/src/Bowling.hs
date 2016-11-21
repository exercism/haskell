module Bowling (bowlingStart, roll, score) where

import Data.List  (tails)

data Bowling = BowlingScore [Frame] |
               BowlingFailure

type Roll = Int

data Frame = Frame { rolls :: [Roll], finalFrame :: Bool }

bowlingStart :: Bowling
bowlingStart = BowlingScore [Frame [] False]

roll :: Bowling -> Int -> Bowling
roll BowlingFailure _ = BowlingFailure
roll _ r | r < 0 || r > 10 = BowlingFailure
roll (BowlingScore (f@(Frame rs True):fs)) r
  | length rs < 2 = BowlingScore $ addRoll f r:fs
  | throws f == 2 && (head rs == 10 || spare f) = addFillBall
  | otherwise = BowlingFailure -- no more rolls possible
  where
    addFillBall :: Bowling
    addFillBall
      | firstRoll == 10 && secondRoll /= 10 && secondRoll + r > 10 = BowlingFailure
      | otherwise = BowlingScore $ addRoll f r:fs
      where [firstRoll, secondRoll] = rolls f
roll (BowlingScore (f@(Frame _ False):fs)) r
  | complete f = BowlingScore $ Frame [r] (length fs == 8):f:fs
  | pins f + r <= 10 = BowlingScore $ addRoll f r:fs
roll _ _ = BowlingFailure

score :: Bowling -> Maybe Int
score BowlingFailure = Nothing
score (BowlingScore fs)
  | length fs < 10 || any (not . complete) fs = Nothing -- incomplete game
  | otherwise = Just $ foldl addScore 0 fsTails
  where
    fsTails = take 10 . tails . reverse $ fs

    addScore :: Int -> [Frame] -> Int
    addScore acc (f:fts) = acc + fscore f fts
    addScore acc [] = acc

-- Frame functions
addRoll :: Frame -> Roll -> Frame
addRoll f r = Frame (rolls f ++ [r]) (finalFrame f)

throws :: Frame -> Int
throws = length . rolls

pins :: Frame -> Int
pins = sum . rolls

strike :: Frame -> Bool
strike f = throws f == 1 && pins f == 10

spare :: Frame -> Bool
spare f = throws f == 2 && pins f == 10

complete :: Frame -> Bool
complete f
  | not $ finalFrame f = strike f || throws f == 2
  | head (rolls f) == 10 || sum (take 2 $ rolls f) == 10 = throws f == 3
  | otherwise = throws f == 2

fscore :: Frame -> [Frame] -> Int
fscore f nextFrames = pins f + bonus
  where
    bonus
      | finalFrame f = 0
      | strike f = nextRoll + nextToNextRoll
      | spare f  = nextRoll
      | otherwise = 0
    nextRoll = head nextFrameRolls
    nextToNextRoll
      | length nextFrameRolls > 1 = head $ tail nextFrameRolls
      | otherwise = head nextToNextFrameRolls
    nextFrameRolls = rolls $ head nextFrames
    nextToNextFrameRolls = rolls . head . tail $ nextFrames
