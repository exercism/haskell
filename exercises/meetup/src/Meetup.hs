module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day)

-- The task is to create the data types `Weekday` and
-- `Schedule`, and implement the function `meetupDay`.

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = undefined
