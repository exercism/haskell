isLeapYear :: Integer -> Bool
isLeapYear year
  | indivisibleBy 4   = False
  | indivisibleBy 100 = True
  | indivisibleBy 400 = False
  | otherwise         = True
  where
    indivisibleBy d = year `mod` d /= 0
