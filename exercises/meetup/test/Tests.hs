{-# LANGUAGE RecordWildCards #-}

import Data.Foldable      (for_)
import Data.Time.Calendar (fromGregorian)
import Test.Hspec         (Spec, describe, it, shouldBe)
import Test.Hspec.Runner  (configFastFail, defaultConfig, hspecWith)

import Meetup (Weekday(..), Schedule(..), meetupDay)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "meetupDay" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion   = returnedDay `shouldBe` expectedDay
        returnedDay = meetupDay week dayofweek year month
        expectedDay = fromGregorian year month dayofmonth

data Case = Case { description :: String
                 , year        :: Integer
                 , month       :: Int
                 , week        :: Schedule
                 , dayofweek   :: Weekday
                 , dayofmonth  :: Int
                 }

cases :: [Case]
cases = [ Case { description = "monteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Monday
               , dayofmonth  = 13
               }
        , Case { description = "monteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Monday
               , dayofmonth  = 19
               }
        , Case { description = "monteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Monday
               , dayofmonth  = 16
               }
        , Case { description = "tuesteenth of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Teenth
               , dayofweek   = Tuesday
               , dayofmonth  = 19
               }
        , Case { description = "tuesteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Tuesday
               , dayofmonth  = 16
               }
        , Case { description = "tuesteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Tuesday
               , dayofmonth  = 13
               }
        , Case { description = "wednesteenth of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Teenth
               , dayofweek   = Wednesday
               , dayofmonth  = 16
               }
        , Case { description = "wednesteenth of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Teenth
               , dayofweek   = Wednesday
               , dayofmonth  = 13
               }
        , Case { description = "wednesteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Wednesday
               , dayofmonth  = 19
               }
        , Case { description = "thursteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Thursday
               , dayofmonth  = 16
               }
        , Case { description = "thursteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Thursday
               , dayofmonth  = 13
               }
        , Case { description = "thursteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Thursday
               , dayofmonth  = 19
               }
        , Case { description = "friteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Friday
               , dayofmonth  = 19
               }
        , Case { description = "friteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Friday
               , dayofmonth  = 16
               }
        , Case { description = "friteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Friday
               , dayofmonth  = 13
               }
        , Case { description = "saturteenth of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Teenth
               , dayofweek   = Saturday
               , dayofmonth  = 16
               }
        , Case { description = "saturteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Saturday
               , dayofmonth  = 13
               }
        , Case { description = "saturteenth of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Teenth
               , dayofweek   = Saturday
               , dayofmonth  = 19
               }
        , Case { description = "sunteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Sunday
               , dayofmonth  = 19
               }
        , Case { description = "sunteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Sunday
               , dayofmonth  = 16
               }
        , Case { description = "sunteenth of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Teenth
               , dayofweek   = Sunday
               , dayofmonth  = 13
               }
        , Case { description = "first Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = First
               , dayofweek   = Monday
               , dayofmonth  = 4
               }
        , Case { description = "first Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = First
               , dayofweek   = Monday
               , dayofmonth  = 1
               }
        , Case { description = "first Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = First
               , dayofweek   = Tuesday
               , dayofmonth  = 7
               }
        , Case { description = "first Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = First
               , dayofweek   = Tuesday
               , dayofmonth  = 4
               }
        , Case { description = "first Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = First
               , dayofweek   = Wednesday
               , dayofmonth  = 3
               }
        , Case { description = "first Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = First
               , dayofweek   = Wednesday
               , dayofmonth  = 7
               }
        , Case { description = "first Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = First
               , dayofweek   = Thursday
               , dayofmonth  = 5
               }
        , Case { description = "first Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = First
               , dayofweek   = Thursday
               , dayofmonth  = 3
               }
        , Case { description = "first Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = First
               , dayofweek   = Friday
               , dayofmonth  = 1
               }
        , Case { description = "first Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = First
               , dayofweek   = Friday
               , dayofmonth  = 6
               }
        , Case { description = "first Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = First
               , dayofweek   = Saturday
               , dayofmonth  = 5
               }
        , Case { description = "first Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = First
               , dayofweek   = Saturday
               , dayofmonth  = 2
               }
        , Case { description = "first Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = First
               , dayofweek   = Sunday
               , dayofmonth  = 3
               }
        , Case { description = "first Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = First
               , dayofweek   = Sunday
               , dayofmonth  = 7
               }
        , Case { description = "second Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Second
               , dayofweek   = Monday
               , dayofmonth  = 11
               }
        , Case { description = "second Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Second
               , dayofweek   = Monday
               , dayofmonth  = 8
               }
        , Case { description = "second Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Second
               , dayofweek   = Tuesday
               , dayofmonth  = 14
               }
        , Case { description = "second Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Second
               , dayofweek   = Tuesday
               , dayofmonth  = 11
               }
        , Case { description = "second Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Second
               , dayofweek   = Wednesday
               , dayofmonth  = 10
               }
        , Case { description = "second Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Second
               , dayofweek   = Wednesday
               , dayofmonth  = 14
               }
        , Case { description = "second Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Second
               , dayofweek   = Thursday
               , dayofmonth  = 12
               }
        , Case { description = "second Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Second
               , dayofweek   = Thursday
               , dayofmonth  = 10
               }
        , Case { description = "second Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Second
               , dayofweek   = Friday
               , dayofmonth  = 8
               }
        , Case { description = "second Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Second
               , dayofweek   = Friday
               , dayofmonth  = 13
               }
        , Case { description = "second Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Second
               , dayofweek   = Saturday
               , dayofmonth  = 12
               }
        , Case { description = "second Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Second
               , dayofweek   = Saturday
               , dayofmonth  = 9
               }
        , Case { description = "second Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Second
               , dayofweek   = Sunday
               , dayofmonth  = 10
               }
        , Case { description = "second Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Second
               , dayofweek   = Sunday
               , dayofmonth  = 14
               }
        , Case { description = "third Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Third
               , dayofweek   = Monday
               , dayofmonth  = 18
               }
        , Case { description = "third Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Third
               , dayofweek   = Monday
               , dayofmonth  = 15
               }
        , Case { description = "third Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Third
               , dayofweek   = Tuesday
               , dayofmonth  = 21
               }
        , Case { description = "third Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Third
               , dayofweek   = Tuesday
               , dayofmonth  = 18
               }
        , Case { description = "third Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Third
               , dayofweek   = Wednesday
               , dayofmonth  = 17
               }
        , Case { description = "third Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Third
               , dayofweek   = Wednesday
               , dayofmonth  = 21
               }
        , Case { description = "third Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Third
               , dayofweek   = Thursday
               , dayofmonth  = 19
               }
        , Case { description = "third Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Third
               , dayofweek   = Thursday
               , dayofmonth  = 17
               }
        , Case { description = "third Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Third
               , dayofweek   = Friday
               , dayofmonth  = 15
               }
        , Case { description = "third Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Third
               , dayofweek   = Friday
               , dayofmonth  = 20
               }
        , Case { description = "third Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Third
               , dayofweek   = Saturday
               , dayofmonth  = 19
               }
        , Case { description = "third Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Third
               , dayofweek   = Saturday
               , dayofmonth  = 16
               }
        , Case { description = "third Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Third
               , dayofweek   = Sunday
               , dayofmonth  = 17
               }
        , Case { description = "third Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Third
               , dayofweek   = Sunday
               , dayofmonth  = 21
               }
        , Case { description = "fourth Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Fourth
               , dayofweek   = Monday
               , dayofmonth  = 25
               }
        , Case { description = "fourth Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Fourth
               , dayofweek   = Monday
               , dayofmonth  = 22
               }
        , Case { description = "fourth Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Fourth
               , dayofweek   = Tuesday
               , dayofmonth  = 28
               }
        , Case { description = "fourth Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Fourth
               , dayofweek   = Tuesday
               , dayofmonth  = 25
               }
        , Case { description = "fourth Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Fourth
               , dayofweek   = Wednesday
               , dayofmonth  = 24
               }
        , Case { description = "fourth Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Fourth
               , dayofweek   = Wednesday
               , dayofmonth  = 28
               }
        , Case { description = "fourth Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Fourth
               , dayofweek   = Thursday
               , dayofmonth  = 26
               }
        , Case { description = "fourth Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Fourth
               , dayofweek   = Thursday
               , dayofmonth  = 24
               }
        , Case { description = "fourth Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Fourth
               , dayofweek   = Friday
               , dayofmonth  = 22
               }
        , Case { description = "fourth Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Fourth
               , dayofweek   = Friday
               , dayofmonth  = 27
               }
        , Case { description = "fourth Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Fourth
               , dayofweek   = Saturday
               , dayofmonth  = 26
               }
        , Case { description = "fourth Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Fourth
               , dayofweek   = Saturday
               , dayofmonth  = 23
               }
        , Case { description = "fourth Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Fourth
               , dayofweek   = Sunday
               , dayofmonth  = 24
               }
        , Case { description = "fourth Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Fourth
               , dayofweek   = Sunday
               , dayofmonth  = 28
               }
        , Case { description = "last Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Last
               , dayofweek   = Monday
               , dayofmonth  = 25
               }
        , Case { description = "last Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Last
               , dayofweek   = Monday
               , dayofmonth  = 29
               }
        , Case { description = "last Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Last
               , dayofweek   = Tuesday
               , dayofmonth  = 28
               }
        , Case { description = "last Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Last
               , dayofweek   = Tuesday
               , dayofmonth  = 25
               }
        , Case { description = "last Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 31
               }
        , Case { description = "last Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 28
               }
        , Case { description = "last Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Last
               , dayofweek   = Thursday
               , dayofmonth  = 26
               }
        , Case { description = "last Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Last
               , dayofweek   = Thursday
               , dayofmonth  = 31
               }
        , Case { description = "last Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Last
               , dayofweek   = Friday
               , dayofmonth  = 29
               }
        , Case { description = "last Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Last
               , dayofweek   = Friday
               , dayofmonth  = 27
               }
        , Case { description = "last Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Last
               , dayofweek   = Saturday
               , dayofmonth  = 26
               }
        , Case { description = "last Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Last
               , dayofweek   = Saturday
               , dayofmonth  = 23
               }
        , Case { description = "last Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Last
               , dayofweek   = Sunday
               , dayofmonth  = 31
               }
        , Case { description = "last Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Last
               , dayofweek   = Sunday
               , dayofmonth  = 28
               }
        , Case { description = "last Wednesday of February 2012"
               , year        = 2012
               , month       = 2
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 29
               }
        , Case { description = "last Wednesday of December 2014"
               , year        = 2014
               , month       = 12
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 31
               }
        , Case { description = "last Sunday of February 2015"
               , year        = 2015
               , month       = 2
               , week        = Last
               , dayofweek   = Sunday
               , dayofmonth  = 22
               }
        , Case { description = "first Friday of December 2012"
               , year        = 2012
               , month       = 12
               , week        = First
               , dayofweek   = Friday
               , dayofmonth  = 7
               }
        ]
