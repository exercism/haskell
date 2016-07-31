{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Binary (toDecimal)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "binary" $
          describe "toDecimal" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = toDecimal binary `shouldBe` fromIntegral expected

-- Test cases adapted from `exercism/x-common/binary.json` on 2016-07-26.
-- Some descriptions mentioning "errors" where changed to avoid confusion.

data Case = Case { description :: String
                 , binary      :: String
                 , expected    :: Integer
                 }

cases :: [Case]
cases = [ Case { description = "binary 0 is decimal 0"
               , binary      = "0"
               , expected    =  0
               }
        , Case { description = "binary 1 is decimal 1"
               , binary      = "1"
               , expected    =  1
               }
        , Case { description = "binary 10 is decimal 2"
               , binary      = "10"
               , expected    =  2
               }
        , Case { description = "binary 11 is decimal 3"
               , binary      = "11"
               , expected    =  3
               }
        , Case { description = "binary 100 is decimal 4"
               , binary      = "100"
               , expected    =  4
               }
        , Case { description = "binary 1001 is decimal 9"
               , binary      = "1001"
               , expected    =  9
               }
        , Case { description = "binary 11010 is decimal 26"
               , binary      = "11010"
               , expected    =  26
               }
        , Case { description = "binary 10001101000 is decimal 1128"
               , binary      = "10001101000"
               , expected    =  1128
               }
        , Case { description = "binary ignores leading zeros"
               , binary      = "000011111"
               , expected    =  31
               }
        , Case { description = "numbers other than one and zero return zero"
               , binary      = "2"
               , expected    =  0
               }
        , Case { description = "numbers other than one and zero return zero"
               , binary      = "012"
               , expected    =  0
               }
        , Case { description = "containing letters returns zero"
               , binary      = "10nope"
               , expected    =  0
               }
        , Case { description = "containing letters returns zero"
               , binary      = "nope10"
               , expected    =  0
               }
        , Case { description = "containing letters returns zero"
               , binary      = "10nope10"
               , expected    =  0
               }
        , Case { description = "containing letters returns zero"
               , binary      = "001 nope"
               , expected    =  0
               }
        ]
