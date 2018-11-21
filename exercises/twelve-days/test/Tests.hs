{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import TwelveDays (recite)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "responseFor" $ for_ cases test
  where
      test Case{..} = it description assertion
        where
          assertion = recite start stop `shouldBe` expected

data Case = Case { description :: String
                 , start       :: Int
                 , stop        :: Int
                 , expected    :: [String]
                 }

cases :: [Case]
cases = [ Case { description = "first day a partridge in a pear tree"
               , start = 1
               , stop = 1
               , expected = [
                   "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "second day two turtle doves"
               , start = 2
               , stop = 2
               , expected = [
                   "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "third day three french hens"
               , start = 3
               , stop = 3
               , expected = [
                   "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "fourth day four calling birds"
               , start = 4
               , stop = 4
               , expected = [
                   "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "fifth day five gold rings"
               , start = 5
               , stop = 5
               , expected = [
                   "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "sixth day six geese-a-laying"
               , start = 6
               , stop = 6
               , expected = [
                   "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "seventh day seven swans-a-swimming"
               , start = 7
               , stop = 7
               , expected = [
                   "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "eighth day eight maids-a-milking"
               , start = 8
               , stop = 8
               , expected = [
                   "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "ninth day nine ladies dancing"
               , start = 9
               , stop = 9
               , expected = [
                   "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "tenth day ten lords-a-leaping"
               , start = 10
               , stop = 10
               , expected = [
                   "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "eleventh day eleven pipers piping"
               , start = 11
               , stop = 11
               , expected = [
                   "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "twelfth day twelve drummers drumming"
               , start = 12
               , stop = 12
               , expected = [
                   "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "recites first three verses of the song"
               , start = 1
               , stop = 3
               , expected = [
                   "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
                 , "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "recites three verses from the middle of the song"
               , start = 4
               , stop = 6
               , expected = [
                   "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        , Case { description = "recites the whole song"
               , start = 1
               , stop = 12
               , expected = [
                   "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
                 , "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 , "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
                 ]
               }
        ]
