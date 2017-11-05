{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Hamming (distance)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "distance" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = expression `shouldBe` fromIntegral <$> expected
        expression = distance strand1 strand2

data Case = Case { description :: String
                 , strand1     :: String
                 , strand2     :: String
                 , expected    :: Maybe Integer
                 }

cases :: [Case]
cases = [ Case { description = "empty strands"
               , strand1     = ""
               , strand2     = ""
               , expected    = Just 0
               }
        , Case { description = "identical strands"
               , strand1     = "A"
               , strand2     = "A"
               , expected    = Just 0
               }
        , Case { description = "long identical strands"
               , strand1     = "GGACTGA"
               , strand2     = "GGACTGA"
               , expected    = Just 0
               }
        , Case { description = "complete distance in single nucleotide strands"
               , strand1     = "A"
               , strand2     = "G"
               , expected    = Just 1
               }
        , Case { description = "complete distance in small strands"
               , strand1     = "AG"
               , strand2     = "CT"
               , expected    = Just 2
               }
        , Case { description = "small distance in small strands"
               , strand1     = "AT"
               , strand2     = "CT"
               , expected    = Just 1
               }
        , Case { description = "small distance"
               , strand1     = "GGACG"
               , strand2     = "GGTCG"
               , expected    = Just 1
               }
        , Case { description = "small distance in long strands"
               , strand1     = "ACCAGGG"
               , strand2     = "ACTATGG"
               , expected    = Just 2
               }
        , Case { description = "non-unique character in first strand"
               , strand1     = "AAG"
               , strand2     = "AAA"
               , expected    = Just 1
               }
        , Case { description = "non-unique character in second strand"
               , strand1     = "AAA"
               , strand2     = "AAG"
               , expected    = Just 1
               }
        , Case { description = "same nucleotides in different positions"
               , strand1     = "TAG"
               , strand2     = "GAT"
               , expected    = Just 2
               }
        , Case { description = "large distance"
               , strand1     = "GATACA"
               , strand2     = "GCATAA"
               , expected    = Just 4
               }
        , Case { description = "large distance in off-by-one strand"
               , strand1     = "GGACGGATTCTG"
               , strand2     = "AGGACGGATTCT"
               , expected    = Just 9
               }
        , Case { description = "disallow first strand longer"
               , strand1     = "AATG"
               , strand2     = "AAA"
               , expected    = Nothing
               }
        , Case { description = "disallow second strand longer"
               , strand1     = "ATA"
               , strand2     = "AGTG"
               , expected    = Nothing
               }
        ]
