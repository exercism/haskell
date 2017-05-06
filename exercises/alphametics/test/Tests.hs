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
specs = describe "alphametics" $
          describe "solve" $ for_ cases test
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
--        , Case { description  = "puzzle with multiplication"
--               , puzzle       = "IF * DR == DORI"
--               , expected     = Just [('I', 8), ('F', 2), ('D', 3), ('R', 9), ('O', 1)]
--               }
--        , Case { description  = "puzzle with exponents"
--               , puzzle       = "PI * R ^ 2 == AREA"
--               , expected     = Just [('P', 9), ('I', 6), ('R', 7), ('A', 4), ('E', 0)]
--               }
        ]

