{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable              (for_)
import Test.Hspec                 (Spec, describe, it, shouldBe)
import Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck            (Gen, elements, forAll)

import ResistorColors (Color (..), value)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "value" $ do
  describe "equality tests" $ for_ cases test

  describe "property tests" $
    it "all values starting with Black are single digit" $
      forAll colorGen (\color -> value (Black, color) < 10)

    -- Add more property tests here
  where
    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = value input `shouldBe` expected

colorGen :: Gen Color
colorGen = elements [minBound ..]

data Case = Case { description :: String
                 , input       :: (Color, Color)
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "Brown and black"
               , input       = (Brown, Black)
               , expected    = 10
               }
        , Case { description = "Blue and grey"
               , input       = (Blue, Grey)
               , expected    = 68
               }
        , Case { description = "Yellow and violet"
               , input       = (Yellow, Violet)
               , expected    = 47
               }
        , Case { description = "Orange and orange"
               , input       = (Orange, Orange)
               , expected    = 33
               }
        -- Note: This test suite omits testing three-color bands,
        -- since they are not representable as (Color, Color). They
        -- are addressed in the exercise resistor-color-trio.
        ]

-- c193c935fe902d4004778872de9e4e61108c271a
