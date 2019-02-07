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
        , Case { description = "single letter identical strands"
               , strand1     = "A"
               , strand2     = "A"
               , expected    = Just 0
               }
        , Case { description = "single letter different strands"
               , strand1     = "G"
               , strand2     = "T"
               , expected    = Just 1
               }
        , Case { description = "long identical strands"
               , strand1     = "GGACTGAAATCTG"
               , strand2     = "GGACTGAAATCTG"
               , expected    = Just 0
               }
        , Case { description = "long different strands"
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
        , Case { description = "disallow left empty strand"
               , strand1     = ""
               , strand2     = "G"
               , expected    = Nothing
               }
        , Case { description = "disallow right empty strand"
               , strand1     = "G"
               , strand2     = ""
               , expected    = Nothing
               }
        ]
