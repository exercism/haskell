{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Say (inEnglish)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "inEnglish" $ for_ cases test
  where

    test (n, expected) = it description assertion
      where
        description = show n
        assertion   = inEnglish n `shouldBe` expected

    cases = [ (            0, Just "zero"                                )
            , (            1, Just "one"                                 )
            , (           14, Just "fourteen"                            )
            , (           20, Just "twenty"                              )
            , (           22, Just "twenty-two"                          )
            , (          100, Just "one hundred"                         )
            , (          123, Just "one hundred twenty-three"            )
            , (         1000, Just "one thousand"                        )
            , (         1234, Just "one thousand two hundred thirty-four")
            , (      1000000, Just "one million"                         )
            , (      1000002, Just "one million two"                     )
            , (      1002345, Just "one million two thousand three \
                                   \hundred forty-five"                  )
            , (   1000000000, Just "one billion"                         )
            , ( 987654321123, Just "nine hundred eighty-seven billion \
                                   \six hundred fifty-four million \
                                   \three hundred twenty-one thousand \
                                   \one hundred twenty-three"            )
            , (           -1, Nothing                                    )
         -- Even though the problem-specifications tests have this case,
         -- we decide not to test it, to give freedom to go to trillions if desired.
         -- , (1000000000000, Nothing                                    )
            ]
