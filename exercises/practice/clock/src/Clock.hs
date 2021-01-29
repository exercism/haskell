module Clock (addDelta, fromHourMin, toString) where

data Clock = Dummy
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = error "You need to implement this function."

toString :: Clock -> String
toString clock = error "You need to implement this function."

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = error "You need to implement this function."
