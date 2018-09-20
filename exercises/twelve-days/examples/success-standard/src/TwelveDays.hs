module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map lyrics [start .. stop]

lyrics :: Int -> String
lyrics day = intro day ++ gifts ++ "."
  where
    gifts = intercalate ", " (map gift [day, day - 1 .. 1])
    gift day' = (if day' == 1 && day > 1 then "and " else "") ++ snd (verse day')

intro :: Int -> String
intro day = "On the " ++ ordinal ++ " day of Christmas my true love gave to me: "
  where (ordinal, _) = verse day

verse :: Int -> (String, String)
verse 12 = ("twelfth", "twelve Drummers Drumming")
verse 11 = ("eleventh", "eleven Pipers Piping")
verse 10 = ("tenth", "ten Lords-a-Leaping")
verse  9 = ("ninth", "nine Ladies Dancing")
verse  8 = ("eighth", "eight Maids-a-Milking")
verse  7 = ("seventh", "seven Swans-a-Swimming")
verse  6 = ("sixth", "six Geese-a-Laying")
verse  5 = ("fifth", "five Gold Rings")
verse  4 = ("fourth", "four Calling Birds")
verse  3 = ("third", "three French Hens")
verse  2 = ("second", "two Turtle Doves")
verse  1 = ("first", "a Partridge in a Pear Tree")
verse  _ = ("", "")
