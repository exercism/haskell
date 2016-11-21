{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Bowling (bowlingStart, roll, score)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "bowling" $
          describe "roll, score" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = result `shouldBe` expected
        result = score $ foldl roll bowlingStart rolls

-- Test cases adapted from `exercism/x-common/bowling` on 2016-11-20.

data Case = Case { description :: String
                 , rolls       :: [Int]
                 , expected    :: Maybe Int
                 }

cases :: [Case]
cases = [ Case { description = "should be able to score a game with all zeros"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 0
               }
        , Case { description = "should be able to score a game with no strikes or spares"
               , rolls       = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]
               , expected    = Just 90
               }
        , Case { description = "a spare followed by zeros is worth ten points"
               , rolls       = [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 10
               }
        , Case { description = "points scored in the roll after a spare are counted twice"
               , rolls       = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 16
               }
        , Case { description = "consecutive spares each get a one roll bonus"
               , rolls       = [5, 5, 3, 7, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 31
               }
        , Case { description = "a spare in the last frame gets a one roll bonus that is counted once"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]
               , expected    = Just 17
               }
        , Case { description = "a strike earns ten points in a frame with a single roll"
               , rolls       = [10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 10
               }
        , Case { description = "points scored in the two rolls after a strike are counted twice as a bonus"
               , rolls       = [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 26
               }
        , Case { description = "consecutive strikes each get the two roll bonus"
               , rolls       = [10, 10, 10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Just 81
               }
        , Case { description = "a strike in the last frame gets a two roll bonus that is counted once"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 1]
               , expected    = Just 18
               }
        , Case { description = "rolling a spare with the two roll bonus does not get a bonus roll"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 3]
               , expected    = Just 20
               }
        , Case { description = "strikes with the two roll bonus do not get bonus rolls"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10]
               , expected    = Just 30
               }
        , Case { description = "a strike with the one roll bonus after a spare in the last frame does not get a bonus"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 10]
               , expected    = Just 20
               }
        , Case { description = "all strikes is a perfect game"
               , rolls       = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
               , expected    = Just 300
               }
        , Case { description = "rolls can not score negative points"
               , rolls       = [-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Nothing
               }
        , Case { description = "a roll can not score more than 10 points"
               , rolls       = [11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Nothing
               }
        , Case { description = "two rolls in a frame can not score more than 10 points"
               , rolls       = [5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Nothing
               }
        , Case { description = "bonus roll after a strike in the last frame can not score more than 10 points"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 11]
               , expected    = Nothing
               }
        , Case { description = "two bonus rolls after a strike in the last frame can not score more than 10 points"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5, 6]
               , expected    = Nothing
               }
        , Case { description = "two bonus rolls after a strike in the last frame can score more than 10 points if one is a strike"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 6]
               , expected    = Just 26
               }
        , Case { description = "the second bonus rolls after a strike in the last frame can not be a strike if the first one is not a strike"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 6, 10]
               , expected    = Nothing
               }
        , Case { description = "second bonus roll after a strike in the last frame can not score than 10 points"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 11]
               , expected    = Nothing
               }
        , Case { description = "an unstarted game can not be scored"
               , rolls       = []
               , expected    = Nothing
               }
        , Case { description = "an incomplete game can not be scored"
               , rolls       = [0, 0]
               , expected    = Nothing
               }
        , Case { description = "a game with more than ten frames can not be scored"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Nothing
               }
        , Case { description = "bonus rolls for a strike in the last frame must be rolled before score can be calculated"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
               , expected    = Nothing
               }
        , Case { description = "both bonus rolls for a strike in the last frame must be rolled before score can be calculated"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]
               , expected    = Nothing
               }
        , Case { description = "bonus roll for a spare in the last frame must be rolled before score can be calculated"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3]
               , expected    = Nothing
               }
        ]
