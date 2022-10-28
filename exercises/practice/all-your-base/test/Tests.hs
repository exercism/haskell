{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Base (Error(..), rebase)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "rebase" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = expression `shouldBe` outputDigits
        expression = rebase inputBase outputBase inputDigits

data Case = Case { description  :: String
                 , inputBase    :: Integer
                 , inputDigits  :: [Integer]
                 , outputBase   :: Integer
                 , outputDigits :: Either (Error Integer) [Integer]
                 }

cases :: [Case]
cases = [ Case { description  = "single bit one to decimal"
               , inputBase    = 2
               , inputDigits  = [1]
               , outputBase   = 10
               , outputDigits = Right [1]
               }
        , Case { description  = "binary to single decimal"
               , inputBase    = 2
               , inputDigits  = [1, 0, 1]
               , outputBase   = 10
               , outputDigits = Right [5]
               }
        , Case { description  = "single decimal to binary"
               , inputBase    = 10
               , inputDigits  = [5]
               , outputBase   = 2
               , outputDigits = Right [1, 0, 1]
               }
        , Case { description  = "binary to multiple decimal"
               , inputBase    = 2
               , inputDigits  = [1, 0, 1, 0, 1, 0]
               , outputBase   = 10
               , outputDigits = Right [4, 2]
               }
        , Case { description  = "decimal to binary"
               , inputBase    = 10
               , inputDigits  = [4, 2]
               , outputBase   = 2
               , outputDigits = Right [1, 0, 1, 0, 1, 0]
               }
        , Case { description  = "trinary to hexadecimal"
               , inputBase    = 3
               , inputDigits  = [1, 1, 2, 0]
               , outputBase   = 16
               , outputDigits = Right [2, 10]
               }
        , Case { description  = "hexadecimal to trinary"
               , inputBase    = 16
               , inputDigits  = [2, 10]
               , outputBase   = 3
               , outputDigits = Right [1, 1, 2, 0]
               }
        , Case { description  = "15-bit integer"
               , inputBase    = 97
               , inputDigits  = [3, 46, 60]
               , outputBase   = 73
               , outputDigits = Right [6, 10, 45]
               }

          -- The following three cases are [0] in all-your-base.json.
          -- Here we use [] to represent the lack of digits, i.e., zero.
        , Case { description  = "empty list"
               , inputBase    = 2
               , inputDigits  = []
               , outputBase   = 10
               , outputDigits = Right []
               }
        , Case { description  = "single zero"
               , inputBase    = 10
               , inputDigits  = [0]
               , outputBase   = 2
               , outputDigits = Right []
               }
        , Case { description  = "multiple zeros"
               , inputBase    = 10
               , inputDigits  = [0, 0, 0]
               , outputBase   = 2
               , outputDigits = Right []
               }

        , Case { description  = "leading zeros"
               , inputBase    = 7
               , inputDigits  = [0, 6, 0]
               , outputBase   = 10
               , outputDigits = Right [4, 2]
               }
        , Case { description  = "input base is one"
               , inputBase    = 1
               , inputDigits  = [0]
               , outputBase   = 10
               , outputDigits = Left InvalidInputBase
               }
        , Case { description  = "input base is zero"
               , inputBase    = 0
               , inputDigits  = []
               , outputBase   = 10
               , outputDigits = Left InvalidInputBase
               }
        , Case { description  = "input base is negative"
               , inputBase    = -2
               , inputDigits  = [1]
               , outputBase   = 10
               , outputDigits = Left InvalidInputBase
               }
        , Case { description  = "negative digit"
               , inputBase    = 2
               , inputDigits  = [1, -1, 1, 0, 1, 0]
               , outputBase   = 10
               , outputDigits = Left (InvalidDigit (-1))
               }
        , Case { description  = "invalid positive digit"
               , inputBase    = 2
               , inputDigits  = [1, 2, 1, 0, 1, 0]
               , outputBase   = 10
               , outputDigits = Left (InvalidDigit 2)
               }
        , Case { description  = "output base is one"
               , inputBase    = 2
               , inputDigits  = [1, 0, 1, 0, 1, 0]
               , outputBase   = 1
               , outputDigits = Left InvalidOutputBase
               }
        , Case { description  = "output base is zero"
               , inputBase    = 10
               , inputDigits  = [7]
               , outputBase   = 0
               , outputDigits = Left InvalidOutputBase
               }
        , Case { description  = "output base is negative"
               , inputBase    = 2
               , inputDigits  = [1]
               , outputBase   = -7
               , outputDigits = Left InvalidOutputBase
               }
        , Case { description  = "both bases are negative"
               , inputBase    = -2
               , inputDigits  = [1]
               , outputBase   = -7
               -- debatable: This could be Left InvalidOutputBase as well.
               , outputDigits = Left InvalidInputBase
               }
        ]
