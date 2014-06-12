module Clock (fromHourMin, clockHour, clockMin, toString) where
import Text.Printf (printf)

newtype Clock = Clock { unClock :: Int } deriving (Show, Eq)

instance Num Clock where
  fromInteger n = Clock (fromInteger (n `mod` (24 * 60)))
  (Clock a) + (Clock b) = fromIntegral (a + b)
  (Clock a) - (Clock b) = fromIntegral (a - b)
  (Clock a) * (Clock b) = fromIntegral (a * b)
  abs = id
  signum = Clock . signum . unClock

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = fromIntegral (h * 60 + m)

clockHour :: Clock -> Int
clockHour = (`div` 60) . unClock

clockMin :: Clock -> Int
clockMin = (`mod` 60) . unClock

toString :: Clock -> String
toString c = printf "%02d:%02d" (clockHour c) (clockMin c)
