isLeapYear :: Integer -> Bool
isLeapYear year =
  if divisibleBy 100
    then divisibleBy 400
    else divisibleBy 4
  where
    divisibleBy d = year `mod` d == 0
