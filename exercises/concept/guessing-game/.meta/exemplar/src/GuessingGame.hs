module GuessingGame (reply) where

reply :: Int -> String
reply 41 = "So close"
reply 42 = "Correct"
reply 43 = "So close"
reply guess
  | guess < 41 = "Too low"
  | otherwise = "Too high"
