{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SumOfMultiples (sumOfMultiples)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "sumOfMultiples" $ for_ cases test
  where
    test Case{..} = it description' assertion
      where
        description' = unwords [show factors, show limit, "--", description]
        assertion   = expression `shouldBe` fromIntegral expected
        expression  = sumOfMultiples (fromIntegral <$> factors)
                                     (fromIntegral     limit  )

data Case = Case { factors     :: [Integer]
                 , limit       ::  Integer
                 , expected    ::  Integer
                 , description ::  String
                 }

cases :: [Case]
cases = [ Case { factors  = [3, 5]
               , limit    = 1
               , expected = 0
               , description = "no multiples within limit"
               }
        , Case { factors  = [3, 5]
               , limit    = 4
               , expected = 3
               , description = "one factor has multiples within limit"
               }
        , Case { factors  = [3]
               , limit    = 7
               , expected = 9
               , description = "more than one multiple within limit"
               }
        , Case { factors  = [3, 5]
               , limit    = 10
               , expected = 23
               , description = "more than one factor with multiples within limit"
               }
        , Case { factors  = [3, 5]
               , limit    = 100
               , expected = 2318
               , description = "each multiple is only counted once"
               }
        , Case { factors  = [3, 5]
               , limit    = 1000
               , expected = 233168
               , description = "a much larger limit"
               }
        , Case { factors  = [7, 13, 17]
               , limit    = 20
               , expected = 51
               , description = "three factors"
               }
        , Case { factors  = [4, 6]
               , limit    = 15
               , expected = 30
               , description = "factors not relatively prime"
               }
        , Case { factors  = [5, 6, 8]
               , limit    = 150
               , expected = 4419
               , description = "some pairs of factors relatively prime and some not"
               }
        , Case { factors  = [5, 25]
               , limit    = 51
               , expected = 275
               , description = "one factor is a multiple of another"
               }
        , Case { factors  = [43, 47]
               , limit    = 10000
               , expected = 2203160
               , description = "much larger factors"
               }
        , Case { factors  = [1]
               , limit    = 100
               , expected = 4950
               , description = "all numbers are multiples of 1"
               }
        , Case { factors  = []
               , limit    = 10000
               , expected = 0
               , description = "no factors means an empty sum"
               }
        , Case { factors  = [0]
               , limit    = 1
               , expected = 0
               , description = "the only multiple of 0 is 0"
               }
        , Case { factors  = [3, 0]
               , limit    = 4
               , expected = 3
               , description = "the factor 0 does not affect the sum of multiples of other factors"
               }
        , Case { factors  = [2, 3, 5, 7, 11]
               , limit    = 10000
               , expected = 39614537
               , description = "solutions using include-exclude must extend to cardinality greater than 3"
               }
        ]
