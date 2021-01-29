{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Bowling (score, BowlingError(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "roll, score" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = result `shouldBe` expected
        result = score rolls

data Case = Case { description :: String
                 , rolls       :: [Int]
                 , expected    :: Either BowlingError Int
                 }

cases :: [Case]
cases = [ Case { description = "should be able to score a game with all zeros"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 0
               }
        , Case { description = "should be able to score a game with no strikes or spares"
               , rolls       = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]
               , expected    = Right 90
               }
        , Case { description = "a spare followed by zeros is worth ten points"
               , rolls       = [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 10
               }
        , Case { description = "points scored in the roll after a spare are counted twice"
               , rolls       = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 16
               }
        , Case { description = "consecutive spares each get a one roll bonus"
               , rolls       = [5, 5, 3, 7, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 31
               }
        , Case { description = "a spare in the last frame gets a one roll bonus that is counted once"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]
               , expected    = Right 17
               }
        , Case { description = "a strike earns ten points in a frame with a single roll"
               , rolls       = [10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 10
               }
        , Case { description = "points scored in the two rolls after a strike are counted twice as a bonus"
               , rolls       = [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 26
               }
        , Case { description = "consecutive strikes each get the two roll bonus"
               , rolls       = [10, 10, 10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Right 81
               }
        , Case { description = "a strike in the last frame gets a two roll bonus that is counted once"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 1]
               , expected    = Right 18
               }
        , Case { description = "rolling a spare with the two roll bonus does not get a bonus roll"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 3]
               , expected    = Right 20
               }
        , Case { description = "strikes with the two roll bonus do not get bonus rolls"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10]
               , expected    = Right 30
               }
        , Case { description = "a strike with the one roll bonus after a spare in the last frame does not get a bonus"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 10]
               , expected    = Right 20
               }
        , Case { description = "all strikes is a perfect game"
               , rolls       = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
               , expected    = Right 300
               }
        , Case { description = "rolls cannot score negative points"
               , rolls       = [-1]
               , expected    = Left $ InvalidRoll 0 (-1)
               }
        , Case { description = "a roll cannot score more than 10 points"
               , rolls       = [11]
               , expected    = Left $ InvalidRoll 0 11
               }
        , Case { description = "two rolls in a frame cannot score more than 10 points"
               , rolls       = [5, 6]
               , expected    = Left $ InvalidRoll 1 6
               }
        , Case { description = "bonus roll after a strike in the last frame cannot score more than 10 points"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 11]
               , expected    = Left $ InvalidRoll 19 11
               }
        , Case { description = "two bonus rolls after a strike in the last frame cannot score more than 10 points"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5, 6]
               , expected    = Left $ InvalidRoll 20 6
               }
        , Case { description = "two bonus rolls after a strike in the last frame can score more than 10 points if one is a strike"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 6]
               , expected    = Right 26
               }
        , Case { description = "the second bonus rolls after a strike in the last frame cannot be a strike if the first one is not a strike"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 6, 10]
               , expected    = Left $ InvalidRoll 20 10
               }
        , Case { description = "second bonus roll after a strike in the last frame cannot score more than 10 points"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 11]
               , expected    = Left $ InvalidRoll 20 11
               }
        , Case { description = "an unstarted game cannot be scored"
               , rolls       = []
               , expected    = Left IncompleteGame
               }
        , Case { description = "an incomplete game cannot be scored"
               , rolls       = [0, 0]
               , expected    = Left IncompleteGame
               }
        , Case { description = "cannot roll if game already has ten frames"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
               , expected    = Left $ InvalidRoll 20 0
               }
        , Case { description = "bonus rolls for a strike in the last frame must be rolled before score can be calculated"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
               , expected    = Left IncompleteGame
               }
        , Case { description = "both bonus rolls for a strike in the last frame must be rolled before score can be calculated"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]
               , expected    = Left IncompleteGame
               }
        , Case { description = "bonus roll for a spare in the last frame must be rolled before score can be calculated"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3]
               , expected    = Left IncompleteGame
               }
        , Case { description = "cannot roll after bonus roll for spare"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 2, 2]
               , expected    = Left $ InvalidRoll 21 2
               }
        , Case { description = "cannot roll after bonus rolls for strike"
               , rolls       = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 3, 2, 2]
               , expected    = Left $ InvalidRoll 21 2
               }
        ]


-- dc82091047d356d19bebe8fd8b2f7c3f89118010
