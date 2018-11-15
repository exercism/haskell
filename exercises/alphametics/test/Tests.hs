{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.Function     (on)
import Data.List         (sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Alphametics (solve)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "solve" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        shouldMatchSolution = shouldBe `on` fmap sort
        assertion = solve puzzle `shouldMatchSolution` expected

data Case = Case { description :: String
                 , puzzle      :: String
                 , expected    :: Maybe[(Char, Int)]
                 }

cases :: [Case]
cases = [ Case { description = "puzzle with three letters"
               , puzzle      = "I + BB == ILL"
               , expected    = Just [('I', 1), ('B', 9), ('L', 0)]
               }
        , Case { description = "solution must have unique value for each letter"
               , puzzle      = "A == B"
               , expected    = Nothing
               }
        , Case { description = "leading zero solution is invalid"
               , puzzle      = "ACA + DD == BD"
               , expected    = Nothing
               }
        , Case { description = "puzzle with two digits final carry"
               , puzzle      = "A + A + A + A + A + A + A + A + A + A + A + B == BCC"
               , expected    = Just [('A', 9), ('B', 1), ('C', 0)]
               }
        , Case { description = "puzzle with four letters"
               , puzzle      = "AS + A == MOM"
               , expected    = Just [('A', 9), ('S', 2), ('M', 1), ('O', 0)]
               }
        , Case { description = "puzzle with six letters"
               , puzzle      = "NO + NO + TOO == LATE"
               , expected    = Just [('N', 7), ('O', 4), ('T', 9), ('L', 1),
                                      ('A', 0), ('E', 2)]
               }
        , Case { description = "puzzle with seven letters"
               , puzzle      = "HE + SEES + THE == LIGHT"
               , expected    = Just [('E', 4), ('G', 2), ('H', 5), ('I', 0),
                                      ('L', 1), ('S', 9), ('T', 7)]
               }
        , Case { description = "puzzle with eight letters"
               , puzzle      = "SEND + MORE == MONEY"
               , expected    = Just [('S', 9), ('E', 5), ('N', 6), ('D', 7), ('M', 1),
                                      ('O', 0), ('R', 8), ('Y', 2)]
               }
        -- some more (and more demanding) tests
--        , Case { description  = "puzzle with ten letters"
--               , puzzle       = "AND + A + STRONG + OFFENSE + AS + A + GOOD == DEFENSE"
--               , expected     = Just [('A', 5), ('D', 3), ('E', 4), ('F', 7), ('G', 8),
--                                      ('N', 0), ('O', 2), ('R', 1), ('S', 6), ('T', 9)]
--               }
--        , Case { description = "puzzle with ten letters and 199 addends"
--               , puzzle      = "THIS + A + FIRE + THEREFORE + FOR + ALL + HISTORIES + I + TELL + A + TALE + THAT + FALSIFIES + ITS + TITLE + TIS + A + LIE + THE + TALE + OF + THE + LAST + FIRE + HORSES + LATE + AFTER + THE + FIRST + FATHERS + FORESEE + THE + HORRORS + THE + LAST + FREE + TROLL + TERRIFIES + THE + HORSES + OF + FIRE + THE + TROLL + RESTS + AT + THE + HOLE + OF + LOSSES + IT + IS + THERE + THAT + SHE + STORES + ROLES + OF + LEATHERS + AFTER + SHE + SATISFIES + HER + HATE + OFF + THOSE + FEARS + A + TASTE + RISES + AS + SHE + HEARS + THE + LEAST + FAR + HORSE + THOSE + FAST + HORSES + THAT + FIRST + HEAR + THE + TROLL + FLEE + OFF + TO + THE + FOREST + THE + HORSES + THAT + ALERTS + RAISE + THE + STARES + OF + THE + OTHERS + AS + THE + TROLL + ASSAILS + AT + THE + TOTAL + SHIFT + HER + TEETH + TEAR + HOOF + OFF + TORSO + AS + THE + LAST + HORSE + FORFEITS + ITS + LIFE + THE + FIRST + FATHERS + HEAR + OF + THE + HORRORS + THEIR + FEARS + THAT + THE + FIRES + FOR + THEIR + FEASTS + ARREST + AS + THE + FIRST + FATHERS + RESETTLE + THE + LAST + OF + THE + FIRE + HORSES + THE + LAST + TROLL + HARASSES + THE + FOREST + HEART + FREE + AT + LAST + OF + THE + LAST + TROLL + ALL + OFFER + THEIR + FIRE + HEAT + TO + THE + ASSISTERS + FAR + OFF + THE + TROLL + FASTS + ITS + LIFE + SHORTER + AS + STARS + RISE + THE + HORSES + REST + SAFE + AFTER + ALL + SHARE + HOT + FISH + AS + THEIR + AFFILIATES + TAILOR + A + ROOFS + FOR + THEIR + SAFE == FORTRESSES"
--               , expected    = Just [('A', 1), ('E', 0), ('F', 5), ('H', 8), ('I', 7),
--                                     ('L', 2), ('O', 6), ('R', 3), ('S', 4), ('T', 9)]
--               }
--        , Case { description  = "puzzle with multiplication"
--               , puzzle       = "IF * DR == DORI"
--               , expected     = Just [('I', 8), ('F', 2), ('D', 3), ('R', 9), ('O', 1)]
--               }
--        , Case { description  = "puzzle with exponents"
--               , puzzle       = "PI * R ^ 2 == AREA"
--               , expected     = Just [('P', 9), ('I', 6), ('R', 7), ('A', 4), ('E', 0)]
--               }
        ]

