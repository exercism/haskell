{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.List         (sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Poker (bestHands)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "bestHands" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = (sort <$> bestHands input) `shouldBe` (sort <$> expected)

data Case = Case { description :: String
                 , input       :: [String]
                 , expected    :: Maybe [String]
                 }

cases :: [Case]
cases = [ Case { description = "single hand always wins"
               , input       = ["4S 5S 7H 8D JC"]
               , expected    = Just ["4S 5S 7H 8D JC"]
               }
        , Case { description = "highest card out of all hands win"
               , input       = [ "4D 5S 6S 8D 3C"
                               , "2S 4C 7S 9H 10H"
                               , "3S 4S 5D 6H JH"]
               , expected    = Just ["3S 4S 5D 6H JH"]
               }
        , Case { description = "a tie has multiple winners"
               , input       = [ "4D 5S 6S 8D 3C"
                               , "2S 4C 7S 9H 10H"
                               , "3S 4S 5D 6H JH"
                               , "3H 4H 5C 6C JD"]
               , expected    = Just [ "3S 4S 5D 6H JH"
                                    , "3H 4H 5C 6C JD"]
               }
        , Case { description =  "multiple hands with the same high cards, tie compares next highest ranked, down to last card"
               , input       = [ "3S 5H 6S 8D 7H"
                               , "2S 5D 6D 8C 7S"]
               , expected    = Just ["3S 5H 6S 8D 7H"]
               }
        , Case { description = "one pair beats high card"
               , input       = [ "4S 5H 6C 8D KH"
                               , "2S 4H 6S 4D JH"]
               , expected    = Just ["2S 4H 6S 4D JH"]
               }
        , Case { description = "highest pair wins"
               , input       = [ "4S 2H 6S 2D JH"
                               , "2S 4H 6C 4D JD"]
               , expected    = Just ["2S 4H 6C 4D JD"]
               }
        , Case { description = "two pairs beats one pair"
               , input       = [ "2S 8H 6S 8D JH"
                               , "4S 5H 4C 8C 5C"]
               , expected    = Just ["4S 5H 4C 8C 5C"]
               }
        , Case { description = "both hands have two pairs, highest ranked pair wins"
               , input       = [ "2S 8H 2D 8D 3H"
                               , "4S 5H 4C 8S 5D"]
               , expected    = Just ["2S 8H 2D 8D 3H"]
               }
        , Case { description = "both hands have two pairs, with the same highest ranked pair, tie goes to low pair"
               , input       = [ "2S QS 2C QD JH"
                               , "JD QH JS 8D QC"]
               , expected    = Just ["JD QH JS 8D QC"]
               }
        , Case { description = "both hands have two identically ranked pairs, tie goes to remaining card (kicker)"
               , input       = [ "JD QH JS 8D QC"
                               , "JS QS JC 2D QD"]
               , expected    = Just ["JD QH JS 8D QC"]
               }
        , Case { description = "three of a kind beats two pair"
               , input       = [ "2S 8H 2H 8D JH"
                               , "4S 5H 4C 8S 4H"]
               , expected    = Just ["4S 5H 4C 8S 4H"]
               }
        , Case { description = "both hands have three of a kind, tie goes to highest ranked triplet"
               , input       = [ "2S 2H 2C 8D JH"
                               , "4S AH AS 8C AD"]
               , expected    = Just ["4S AH AS 8C AD"]
               }
        , Case { description = "with multiple decks, two players can have same three of a kind, ties go to highest remaining cards"
               , input       = [ "4S AH AS 7C AD"
                               , "4S AH AS 8C AD"]
               , expected    = Just ["4S AH AS 8C AD"]
               }
        , Case { description = "a straight beats three of a kind"
               , input       = [ "4S 5H 4C 8D 4H"
                               , "3S 4D 2S 6D 5C"]
               , expected    = Just ["3S 4D 2S 6D 5C"]
               }
        , Case { description = "aces can end a straight (10 J Q K A)"
               , input       = [ "4S 5H 4C 8D 4H"
                               , "10D JH QS KD AC"]
               , expected    = Just ["10D JH QS KD AC"]
               }
        , Case { description = "aces can start a straight (A 2 3 4 5)"
               , input       = [ "4S 5H 4C 8D 4H"
                               , "4D AH 3S 2D 5C"]
               , expected    = Just ["4D AH 3S 2D 5C"]
               }
        , Case { description = "both hands with a straight, tie goes to highest ranked card"
               , input       = [ "4S 6C 7S 8D 5H"
                               , "5S 7H 8S 9D 6H"]
               , expected    = Just ["5S 7H 8S 9D 6H"]
               }
        , Case { description = "even though an ace is usually high, a 5-high straight is the lowest-scoring straight"
               , input       = [ "2H 3C 4D 5D 6H"
                               , "4S AH 3S 2D 5H"]
               , expected    = Just ["2H 3C 4D 5D 6H"]
               }
        , Case { description = "flush beats a straight"
               , input       =  [ "4C 6H 7D 8D 5H"
                                , "2S 4S 5S 6S 7S"]
               , expected    = Just ["2S 4S 5S 6S 7S"]
               }
        , Case { description = "both hands have a flush, tie goes to high card, down to the last one if necessary"
               , input       = [ "4H 7H 8H 9H 6H"
                               , "2S 4S 5S 6S 7S"]
               , expected    = Just ["4H 7H 8H 9H 6H"]
               }
        , Case { description = "full house beats a flush"
               , input       =  [ "3H 6H 7H 8H 5H"
                                , "4S 5H 4C 5D 4H"]
               , expected    = Just ["4S 5H 4C 5D 4H"]
               }
        , Case { description = "both hands have a full house, tie goes to highest-ranked triplet"
               , input       = [ "4H 4S 4D 9S 9D"
                               , "5H 5S 5D 8S 8D"]
               , expected    = Just ["5H 5S 5D 8S 8D"]
               }
        , Case { description = "with multiple decks, both hands have a full house with the same triplet, tie goes to the pair"
               , input       = [ "5H 5S 5D 9S 9D"
                               , "5H 5S 5D 8S 8D"]
               , expected    = Just ["5H 5S 5D 9S 9D"]
               }
        , Case { description = "four of a kind beats a full house"
               , input       =  [ "4S 5H 4D 5D 4H"
                                , "3S 3H 2S 3D 3C"]
               , expected    = Just ["3S 3H 2S 3D 3C"]
               }
        , Case { description = "both hands have four of a kind, tie goes to high quad"
               , input       = [ "2S 2H 2C 8D 2D"
                               , "4S 5H 5S 5D 5C"]
               , expected    = Just ["4S 5H 5S 5D 5C"]
               }
        , Case { description = "with multiple decks, both hands with identical four of a kind, tie determined by kicker"
               , input       = [ "3S 3H 2S 3D 3C"
                               , "3S 3H 4S 3D 3C"]
               , expected    = Just ["3S 3H 4S 3D 3C"]
               }
        , Case { description = "straight flush beats four of a kind"
               , input       = [ "4S 5H 5S 5D 5C"
                               , "7S 8S 9S 6S 10S"]
               , expected    = Just ["7S 8S 9S 6S 10S"]
               }
        , Case { description = "both hands have straight flush, tie goes to highest-ranked card"
               , input       = [ "4H 6H 7H 8H 5H"
                               , "5S 7S 8S 9S 6S"]
               , expected    = Just ["5S 7S 8S 9S 6S"]
               }
        , Case { description = "no winner if a hand is invalid"
               , input       = [ "2H 2S 2D 2C 10XXXXXXH" ]
               , expected    = Nothing
               }
        ]
