module GuessingGame (reply) where

guess :: Int
guess = 42

reply :: Int -> String
reply n
  | guess == n = "Correct"
  | guess + 1 == n || guess - 1 == n = "So close"
  | guess < n = "Too high"
  | guess > n = "Too low"
  | otherwise = "Incorrect" -- catch-all clause to make pattern matching exhaustive
