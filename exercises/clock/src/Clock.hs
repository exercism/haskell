module Clock (addDelta, clockHour, clockMin, fromHourMin, toString) where

data Clock = Dummy

clockHour :: Clock -> Int
clockHour clock = error "You need to implement this function."

clockMin :: Clock -> Int
clockMin clock = error "You need to implement this function."

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = error "You need to implement this function."

toString :: Clock -> String
toString clock = error "You need to implement this function."

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = error "You need to implement this function."
