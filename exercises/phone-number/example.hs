module Phone (number, areaCode, prettyPrint) where
import Data.Char (isDigit)

number :: String -> Maybe String
number input
  | len == 10 = Just digits
  | len == 11 && head digits == '1' = Just $ tail digits
  | otherwise = Nothing
  where digits = filter isDigit input
        len = length digits

parts :: String -> Maybe (String, String, String)
parts input = case number input of
    Nothing   -> Nothing
    Just digits -> Just (ac, exchange, subscriber)
      where
        (ac, exchangeSubscriber) = splitAt 3 digits
        (exchange, subscriber) = splitAt 3 exchangeSubscriber

areaCode :: String -> Maybe String
areaCode input = case parts input of
  Just (ac, _, _) -> Just ac
  Nothing         -> Nothing

prettyPrint :: String -> Maybe String
prettyPrint input = case parts input of
  Just (ac, exchange, subscriber) -> Just $ "(" ++ ac ++ ") " ++ exchange ++ "-" ++ subscriber
  Nothing                         -> Nothing
