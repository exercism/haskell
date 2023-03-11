{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Luhn (isValid)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "valid" $ for_ cases test
  where
    test Case{..} = it description $ isValid input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "single digit strings can not be valid"
               , input       = "1"
               , expected    = False
               }
        , Case { description = "a single zero is invalid"
               , input       = "0"
               , expected    = False
               }
        , Case { description = "a simple valid SIN that remains valid if reversed"
               , input       = "059"
               , expected    = True
               }
        , Case { description = "a simple valid SIN that becomes invalid if reversed"
               , input       = "59"
               , expected    = True
               }
        , Case { description = "a valid Canadian SIN"
               , input       = "055 444 285"
               , expected    = True
               }
        , Case { description = "invalid Canadian SIN"
               , input       = "055 444 286"
               , expected    = False
               }
        , Case { description = "invalid credit card"
               , input       = "8273 1232 7352 0569"
               , expected    = False
               }
        , Case { description = "invalid long number with an even remainder"
               , input       = "1 2345 6789 1234 5678 9012"
               , expected    = False
               }
        , Case { description = "valid number with an even number of digits"
               , input       = "095 245 88"
               , expected    = True
               }
        , Case { description = "valid number with an odd number of spaces"
               , input       = "234 567 891 234"
               , expected    = True
               }
-- This track is not testing these cases, since we would rather focus on the algorithm,
-- and because it seems strange to be unable to distinguish between well-formed invalid input and malformed input.
--        , Case { description = "valid strings with a non-digit added at the end become invalid"
--               , input       = "059a"
--               , expected    = False
--               }
--        , Case { description = "valid strings with punctuation included become invalid"
--               , input       = "055-444-285"
--               , expected    = False
--               }
--        , Case { description = "valid strings with symbols included become invalid"
--               , input       = "055£ 444$ 285"
--               , expected    = False
--               }
        , Case { description = "single zero with space is invalid"
               , input       = " 0"
               , expected    = False
               }
        , Case { description = "more than a single zero is valid"
               , input       = "0000 0"
               , expected    = True
               }
        , Case { description = "input digit 9 is correctly converted to output digit 9"
               , input       = "091"
               , expected    = True
               }
--        , Case { description = "using ascii value for non-doubled non-digit isn't allowed"
--               , input       = "055b 444 285"
--               , expected    = False
--               }
--        , Case { description = "using ascii value for doubled non-digit isn't allowed"
--               , input       = ":9"
--               , expected    = False
--               }
        ]
