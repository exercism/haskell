module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop
  | start <= stop = lyrics start : recite (start + 1) stop
  | otherwise     = []

lyrics :: Int -> String
lyrics days =
  beginning ++ end days
  where
    beginning =
      let (day, _) = verse days
      in "On the " ++ day ++ " day of Christmas my true love gave to me"
    end days'
      | days < 13 && days' > 0 = let (_, gift) = verse days'
                                 in prefix days' ++ gift ++ end (days' - 1)
      | otherwise              = "."
      where
        prefix 1 = if days > 1 then ", and " else ", "
        prefix _ = ", "

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
