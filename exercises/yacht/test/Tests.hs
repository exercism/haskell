{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Yacht (yacht, Category(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "yacht" $ for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion = yacht category dice `shouldBe` expected

data Case = Case { description :: String
                 , dice        :: [Int]
                 , category    :: Category
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "Yacht"
               , dice = [5, 5, 5, 5, 5]
               , category = Yacht
               , expected = 50
               }
        , Case { description = "Not Yacht"
               , dice = [1, 3, 3, 2, 5]
               , category = Yacht
               , expected = 0
               }
        , Case { description = "Ones"
               , dice = [1, 1, 1, 3, 5]
               , category = Ones
               , expected = 3
               }
        , Case { description = "Ones, out of order"
               , dice = [3, 1, 1, 5, 1]
               , category = Ones
               , expected = 3
               }
        , Case { description = "No ones"
               , dice = [4, 3, 6, 5, 5]
               , category = Ones
               , expected = 0
               }
        , Case { description = "Twos"
               , dice = [2, 3, 4, 5, 6]
               , category = Twos
               , expected = 2
               }
        , Case { description = "Fours"
               , dice = [1, 4, 1, 4, 1]
               , category = Fours
               , expected = 8
               }
        , Case { description = "Yacht counted as threes"
               , dice = [3, 3, 3, 3, 3]
               , category = Threes
               , expected = 15
               }
        , Case { description = "Yacht of 3s counted as fives"
               , dice = [3, 3, 3, 3, 3]
               , category = Fives
               , expected = 0
               }
        , Case { description = "Sixes"
               , dice = [2, 3, 4, 5 ,6]
               , category = Sixes
               , expected = 6
               }
        , Case { description = "Full house two small, three big"
               , dice = [2, 2, 4, 4, 4]
               , category = FullHouse
               , expected = 16
               }
        , Case { description = "Full house three small, two big"
               , dice = [5, 3, 3, 5, 3]
               , category = FullHouse
               , expected = 19
               }
        , Case { description = "Two pair is not a full house"
               , dice = [2, 2, 4, 4, 5]
               , category = FullHouse
               , expected = 0
               }
        , Case { description = "Four of a kind is not a full house"
               , dice = [1, 4, 4, 4, 4]
               , category = FullHouse
               , expected = 0
               }
        , Case { description = "Yacht is not a full house"
               , dice = [2, 2, 2, 2, 2]
               , category = FullHouse
               , expected = 0
               }
        , Case { description = "Four of a Kind"
               , dice = [6, 6, 4, 6, 6]
               , category = FourOfAKind
               , expected = 24
        }
        , Case { description = "Yacht can be scored as Four of a Kind"
               , dice = [3, 3, 3, 3, 3]
               , category = FourOfAKind
               , expected = 12
               }
        , Case { description = "Full house is not Four of a Kind"
               , dice = [3, 3, 3, 5, 5]
               , category = FourOfAKind
               , expected = 0
               }
        , Case { description = "Little Straight"
               , dice = [3, 5, 4, 1, 2]
               , category = LittleStraight
               , expected = 30
               }
        , Case { description = "Little Straight as Big Straight"
               , dice = [1, 2, 3, 4, 5]
               , category = BigStraight
               , expected = 0
               }
        , Case { description = "Four in order but not a little straight"
               , dice = [1, 1, 2, 3, 4]
               , category = LittleStraight
               , expected = 0
               }
        , Case { description = "No pairs but not a little straight"
               , dice = [1, 2, 3, 4, 6]
               , category = LittleStraight
               , expected = 0
               }
        , Case { description = "Minimum is 1, maximum is 5, but not a little straight"
               , dice = [1, 1, 3, 4, 5]
               , category = LittleStraight
               , expected = 0
               }
        , Case { description = "Big Straight"
               , dice = [4, 6, 2, 5, 3]
               , category = BigStraight
               , expected = 30
               }
        , Case { description = "Big Straight as little straight"
               , dice = [6, 5, 4, 3, 2]
               , category = LittleStraight
               , expected = 0
               }
        , Case { description = "No pairs but not a big straight"
               , dice = [6, 5, 4, 3, 1]
               , category = BigStraight
               , expected = 0
               }
        , Case { description = "Choice"
               , dice = [3, 3, 5, 6, 6]
               , category = Choice
               , expected = 23
               }
        , Case { description = "Yacht as choice"
               , dice = [2, 2, 2, 2, 2]
               , category = Choice
               , expected = 10
               }
        ]
