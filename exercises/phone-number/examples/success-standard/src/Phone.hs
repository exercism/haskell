module Phone (number) where
import Data.Char (isDigit)

number :: String -> Maybe String
number input = clean input >>= check

check :: String -> Maybe String
check ('0':_) = Nothing
check ('1':_) = Nothing
check (_:_:_:'0':_) = Nothing
check (_:_:_:'1':_) = Nothing
check s = Just s

clean :: String -> Maybe String
clean input
  | len == 10 = Just digits
  | len == 11 && head digits == '1' = Just $ tail digits
  | otherwise = Nothing
  where digits = filter isDigit input
        len = length digits
