module Phone (number) where
import Data.Char (isDigit, isLetter)

number :: String -> Maybe String
number input
  | any isLetter input = Nothing
  | len == 10 = Just digits
  | len == 11 && head digits == '1' = Just $ tail digits
  | otherwise = Nothing
  where digits = filter isDigit input
        len = length digits
