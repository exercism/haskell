module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = undefined
