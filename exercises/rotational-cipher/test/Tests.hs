{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import RotationalCipher (rotate)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "rotate" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = rotate number phrase `shouldBe` expected

data Case = Case { description :: String
                 , number      :: Int
                 , phrase      :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "rotate a by 0, same output as input"
               , number      = 0
               , phrase      = "a"
               , expected    = "a"
               }
        , Case { description = "rotate a by 1"
               , number      = 1
               , phrase      = "a"
               , expected    = "b"
               }
        , Case { description = "rotate a by 26, same output as input"
               , number      = 26
               , phrase      = "a"
               , expected    = "a"
               }
        , Case { description = "rotate m by 13"
               , number      = 13
               , phrase      = "m"
               , expected    = "z"
               }
        , Case { description = "rotate n by 13 with wrap around alphabet"
               , number      = 13
               , phrase      = "n"
               , expected    = "a"
               }
        , Case { description = "rotate capital letters"
               , number      = 5
               , phrase      = "OMG"
               , expected    = "TRL"
               }
        , Case { description = "rotate spaces"
               , number      = 5
               , phrase      = "O M G"
               , expected    = "T R L"
               }
        , Case { description = "rotate numbers"
               , number      = 4
               , phrase      = "Testing 1 2 3 testing"
               , expected    = "Xiwxmrk 1 2 3 xiwxmrk"
               }
        , Case { description = "rotate punctuation"
               , number      = 21
               , phrase      = "Let's eat, Grandma!"
               , expected    = "Gzo'n zvo, Bmviyhv!"
               }
        , Case { description = "rotate all letters"
               , number      = 13
               , phrase      = "The quick brown fox jumps over the lazy dog."
               , expected    = "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
               }
        ]
