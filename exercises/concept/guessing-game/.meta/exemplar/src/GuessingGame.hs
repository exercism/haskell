module GuessingGame (reply) where

reply :: Int -> Int -> String
reply n guess
  | guess == n = "Correct"
  | guess + 1 == n || guess - 1 == n = "So close!"
  | guess < n = "Too low"
  | guess > n = "Too high"
