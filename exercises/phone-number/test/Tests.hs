{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Phone (number)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "phone-number" $
          describe "number" $ for_ cases test
  where
    test Case{..} = it description $ number input `shouldBe` expected

-- Test cases adapted from `exercism/x-common/phone-number` on 2017-01-31.

data Case = Case { description ::       String
                 , input       ::       String
                 , expected    :: Maybe String
                 }

cases :: [Case]
cases =
    [ Case { description = "cleans the number"
           , input       = "(123) 456-7890"
           , expected    = Just "1234567890"
           }
    , Case { description = "cleans number with dots"
           , input       = "123.456.7890"
           , expected    = Just "1234567890"
           }
    , Case { description = "cleans numbers with multiple spaces"
           , input       = "123 456   7890   "
           , expected    = Just "1234567890"
           }
    , Case { description = "invalid when 9 digits"
           , input       = "123456789"
           , expected    = Nothing
           }
    , Case { description = "invalid when 11 digits"
           , input       = "21234567890"
           , expected    = Nothing
           }
    , Case { description = "valid when 11 digits and first is 1"
           , input       = "11234567890"
           , expected    = Just "1234567890"
           }
    , Case { description = "invalid when 12 digits"
           , input       = "321234567890"
           , expected    = Nothing
           }
    , Case { description = "invalid with letters"
           , input       = "123-abc-7890"
           , expected    = Nothing
           }
    , Case { description = "invalid with punctuations"
           , input       = "123-@:!-7890"
           , expected    = Nothing
           }
    , Case { description = "invalid with right number of digits but letters mixed in"
           , input       = "1a2b3c4d5e6f7g8h9i0j"
           , expected    = Nothing
           }
    ]
