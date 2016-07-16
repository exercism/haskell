module Clock (clockHour, clockMin, fromHourMin, toString) where

-- The task is to create the data type `Clock`, with `Eq`,
-- `Show` and `Num` instances, and implement the functions below.
-- The function `fromInteger`, from `Num`, must converts minutes
-- to 24 hour clock time. It is not necessary to have a sensible
-- implementation of `abs` or `signum`.

clockHour :: Clock -> Int
clockHour = undefined

clockMin :: Clock -> Int
clockMin = undefined

fromHourMin :: Int -> Int -> Clock
fromHourMin = undefined

toString :: Clock -> String
toString = undefined
