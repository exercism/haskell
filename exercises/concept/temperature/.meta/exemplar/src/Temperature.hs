module Temperature (tempToC, tempToF) where

tempToC :: Integer -> Float
tempToC fahrenheit = (fromIntegral fahrenheit - 32.00) / 1.80

tempToF :: Float -> Integer
tempToF celsius = ceiling (celsius * 1.80) + 32
