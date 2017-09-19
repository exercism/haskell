module CollatzConjecture (collatz) where

collatzHelper :: Integer -> Integer -> Maybe Integer
collatzHelper t x | x == 1    = Just t
                  | even x    = collatzHelper (t+1) (x `div` 2)
                  | otherwise = collatzHelper (t+1) (x*3 + 1)

collatz :: Integer -> Maybe Integer
collatz x = if x <= 0 then Nothing else collatzHelper 0 x
