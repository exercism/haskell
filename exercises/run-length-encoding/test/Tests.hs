{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import RunLength (encode, decode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "run-length-encoding" $ do
          describe "decode" $ for_ decodeCases $ test decode
          describe "encode" $ for_ encodeCases $ test encode
          describe "both"   $ for_ bothCases   $ test (decode . encode)
  where
    test f Case{..} = it description $ f input `shouldBe` expected

-- Test cases adapted from file
-- `exercism/x-common/exercises/run-length-encoding/canonical-data.json`
-- on 2016-12-26.

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

encodeCases :: [Case]
encodeCases =
    [ Case { description = "encode empty string"
           , input       = ""
           , expected    = ""
           }
    , Case { description = "encode single characters only"
           , input       = "XYZ"
           , expected    = "XYZ"
           }
    , Case { description = "encode simple"
           , input       = "AABBBCCCC"
           , expected    = "2A3B4C"
           }
    , Case { description = "encode with single values"
           , input       = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
           , expected    = "12WB12W3B24WB"
           }
    ]

decodeCases :: [Case]
decodeCases =
    [ Case { description = "decode empty string"
           , input       = ""
           , expected    = ""
           }
    , Case { description = "decode single characters only"
           , input       = "XYZ"
           , expected    = "XYZ"
           }
    , Case { description = "decode simple"
           , input       = "2A3B4C"
           , expected    = "AABBBCCCC"
           }
    , Case { description = "decode with single values"
           , input       = "12WB12W3B24WB"
           , expected    = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
           }
    ]

bothCases :: [Case]
bothCases =
    [ Case { description = "decode . encode combination"
           , input       = "zzz ZZ  zZ"
           , expected    = "zzz ZZ  zZ"
           }
    ]
