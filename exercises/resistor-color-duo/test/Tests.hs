{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable              (for_)
import Test.Hspec                 (Spec, describe, context, it, shouldBe)
import Test.Hspec.Runner          (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck            (property)
import Test.QuickCheck.Gen        (oneof)
import Test.QuickCheck.Arbitrary

import ResistorColors (Color(..), value)

instance Arbitrary Color where
  arbitrary = oneof $ map return [Black, Brown, Red, Orange, Yellow, Green, Blue, Violet, Grey, White]

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "value" $ do
  context "equality tests" $ for_ cases test

  context "property tests" $
    it "all values starting with Black are single digit" $ property $
      \color -> value (Black, color) < 10
    -- Add more property tests here

  where
    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = value input `shouldBe` expected

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
