{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Phone (areaCode, number, prettyPrint)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "phone-number" $ do
          describe "number"      $ for_ numberCases      $ test number
          describe "areaCode"    $ for_ areaCodeCases    $ test areaCode
          describe "prettyPrint" $ for_ prettyPrintCases $ test prettyPrint
  where
    test f Case{..} = it description $ f input `shouldBe` expected

-- As of 2016-07-27, there was no reference file
-- for the test cases in `exercism/x-common`.

data Case = Case { description ::       String
                 , input       ::       String
                 , expected    :: Maybe String
                 }

numberCases :: [Case]
numberCases =
    [ Case { description = "cleans number"
           , input       = "(123) 456-7890"
           , expected    = Just "1234567890"
           }
    , Case { description = "cleans another number"
           , input       = "(612) 555-1212"
           , expected    = Just "6125551212"
           }
    , Case { description = "cleans number with dots"
           , input       = "123.456.7890"
           , expected    = Just "1234567890"
           }
    , Case { description = "cleans another number with dots"
           , input       = "918.765.4321"
           , expected    = Just "9187654321"
           }
    , Case { description = "valid when 11 digits and first is 1"
           , input       = "12468013579"
           , expected    = Just "2468013579"
           }
    , Case { description = "invalid when 11 digits"
           , input       = "21234567890"
           , expected    = Nothing
           }
    , Case { description = "invalid when 9 digits"
           , input       = "123456789"
           , expected    = Nothing
           }
    , Case { description = "invalid when 12 digits"
           , input       = "123456789012"
           , expected    = Nothing
           }
    , Case { description = "invalid when empty"
           , input       = ""
           , expected    = Nothing
           }
    , Case { description = "invalid when no digits present"
           , input       = " (-) "
           , expected    = Nothing
           }
    , Case { description = "valid with leading characters"
           , input       = "my number is 235 813 2134"
           , expected    = Just "2358132134"
           }
    , Case { description = "valid with trailing characters"
           , input       = "987 654 3210 - bob"
           , expected    = Just "9876543210"
           }
    , Case { description = "valid amidst text and punctuation"
           , input       = "Here it is: 415-888-0000. Thanks!"
           , expected    = Just "4158880000"
           }
    ]

areaCodeCases :: [Case]
areaCodeCases =
    [ Case { description = "area code"
           , input       = "1234567890"
           , expected    = Just "123"
           }
    , Case { description = "area code with parentheses"
           , input       = "(612) 555-1212"
           , expected    = Just "612"
           }
    , Case { description = "area code with leading characters"
           , input       = "my number is 235 813 2134"
           , expected    = Just "235"
           }
    , Case { description = "invalid area code"
           , input       = " (-) "
           , expected    = Nothing
           }
    ]

prettyPrintCases :: [Case]
prettyPrintCases =
    [ Case { description = "pretty print"
           , input       = "1234567890"
           , expected    = Just "(123) 456-7890"
           }
    , Case { description = "pretty print with full US phone number"
           , input       = "12345678901"
           , expected    = Just "(234) 567-8901"
           }
    , Case { description = "pretty print amidst text and punctuation"
           , input       = "Here it is: 415-888-0000. Thanks!"
           , expected    = Just "(415) 888-0000"
           }
    ]
