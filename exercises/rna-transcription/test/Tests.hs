{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import DNA (toRNA)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "toRNA" $ for_ cases test
  where
    test Case{..} = it description $ toRNA dna `shouldBe` expected

data Case = Case { description :: String
                 , dna         :: String
                 , expected    :: Either Char String
                 }

cases :: [Case]
cases = [ Case { description = "Empty RNA sequence"
               , dna         = ""
               , expected    = Right ""
               }
        , Case { description = "RNA complement of cytosine is guanine"
               , dna         = "C"
               , expected    = Right "G"
               }
        , Case { description = "RNA complement of guanine is cytosine"
               , dna         = "G"
               , expected    = Right "C"
               }
        , Case { description = "RNA complement of thymine is adenine"
               , dna         = "T"
               , expected    = Right "A"
               }
        , Case { description = "RNA complement of adenine is uracil"
               , dna         = "A"
               , expected    = Right "U"
               }
        , Case { description = "RNA complement"
               , dna         = "ACGTGGTCTTAA"
               , expected    = Right "UGCACCAGAAUU"
               }
        , Case { description = "correctly handles invalid input (RNA instead of DNA)"
               , dna         = "U"
               , expected    = Left 'U'
               }
        , Case { description = "correctly handles completely invalid DNA input"
               , dna         = "XXX"
               , expected    = Left 'X'
               }
        , Case { description = "correctly handles partially invalid DNA input"
               , dna         = "ACGTXXXCTTAA"
               , expected    = Left 'X'
               }
        ]
