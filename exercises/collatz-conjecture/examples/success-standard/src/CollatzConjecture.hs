module CollatzConjecture (collatz) where

collatzHelper :: Integer -> Integer -> Integer
collatzHelper t x | x == 1    = t
                  | even x    = collatzHelper (t+1) (x `div` 2)
                  | otherwise = collatzHelper (t+1) (x*3 + 1)

collatz :: Integer -> Integer
collatz x = if x <= 0 then error "Only positive numbers are allowed"
                      else collatzHelper 0 x
