module Clock (addDelta, fromHourMin, toString) where
import Text.Printf (printf)

newtype Clock = Clock { unClock :: Int } deriving (Eq)

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m (Clock a) = fromInt (a + h * 60 + m)

fromInt :: Int -> Clock
fromInt n = Clock (n `mod` (24 * 60))

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = fromInt (h * 60 + m)

clockHour :: Clock -> Int
clockHour = (`div` 60) . unClock

clockMin :: Clock -> Int
clockMin = (`mod` 60) . unClock

toString :: Clock -> String
toString c = printf "%02d:%02d" (clockHour c) (clockMin c)
