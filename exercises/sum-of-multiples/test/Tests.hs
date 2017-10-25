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
    test Case{..} = it description assertion
      where
        description = unwords [show factors, show limit]
        assertion   = expression `shouldBe` fromIntegral expected
        expression  = sumOfMultiples (fromIntegral <$> factors)
                                     (fromIntegral     limit  )

data Case = Case { factors  :: [Integer]
                 , limit    ::  Integer
                 , expected ::  Integer
                 }

cases :: [Case]
cases = [ Case { factors  = [3, 5]
               , limit    = 1
               , expected = 0
               }
        , Case { factors  = [3, 5]
               , limit    = 4
               , expected = 3
               }
        , Case { factors  = [3]
               , limit    = 7
               , expected = 9
               }
        , Case { factors  = [3, 5]
               , limit    = 10
               , expected = 23
               }
        , Case { factors  = [3, 5]
               , limit    = 100
               , expected = 2318
               }
        , Case { factors  = [3, 5]
               , limit    = 1000
               , expected = 233168
               }
        , Case { factors  = [7, 13, 17]
               , limit    = 20
               , expected = 51
               }
        , Case { factors  = [4, 6]
               , limit    = 15
               , expected = 30
               }
        , Case { factors  = [5, 6, 8]
               , limit    = 150
               , expected = 4419
               }
        , Case { factors  = [5, 25]
               , limit    = 51
               , expected = 275
               }
        , Case { factors  = [43, 47]
               , limit    = 10000
               , expected = 2203160
               }
        , Case { factors  = [1]
               , limit    = 100
               , expected = 4950
               }
        , Case { factors  = []
               , limit    = 10000
               , expected = 0
               }
        ]
