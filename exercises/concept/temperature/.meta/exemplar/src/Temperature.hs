module Temperature (tempTo) where

{- Implement the function `tempTo` to convert a temperature
to either Celsius or Fahrenheit given a character to indicate
the units desired, and a integer in the other unit.          -}

tempTo :: Char -> Integer -> Float
tempTo unit temp
    | unit == 'f' || unit == 'F' = ((fromInteger temp) * 1.80) + 32.00
    | unit == 'c' || unit == 'C' = ((fromInteger temp) - 32.00) / 1.80
    | otherwise = error "Incorrect temperature unit. Use 'C' or 'F'."
