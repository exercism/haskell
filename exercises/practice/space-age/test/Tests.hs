{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.Function     (on)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import SpaceAge (Planet(..), ageOn)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "ageOn" $ for_ cases test
  where
    -- Here we used `fromIntegral`, `fromRational` and `toRational` to
    -- generalize the test suite, allowing any function that takes a
    -- `Planet` and a number, returning an instance of `Real`.
    test Case{..} = it description $ expression `shouldBeAround` expected
      where
        expression = fromRational
                   . toRational
                   . ageOn planet
                   . fromIntegral
                   $ seconds
        shouldBeAround = shouldBe `on` roundTo 2
        roundTo n = (/ 10 ^ n) . fromIntegral . round . (* 10 ^ n)

data Case = Case { description :: String
                 , planet      :: Planet
                 , seconds     :: Integer
                 , expected    :: Double
                 }

cases :: [Case]
cases = [ Case { description = "Earth"
               , planet      = Earth
               , seconds     = 1000000000
               , expected    = 31.69
               }
        , Case { description = "Mercury"
               , planet      = Mercury
               , seconds     = 2134835688
               , expected    = 280.88
               }
        , Case { description = "Venus"
               , planet      = Venus
               , seconds     = 189839836
               , expected    = 9.78
               }
        , Case { description = "Mars"
               , planet      = Mars
               , seconds     = 2129871239
               , expected    = 35.88
               }
        , Case { description = "Jupiter"
               , planet      = Jupiter
               , seconds     = 901876382
               , expected    = 2.41
               }
        , Case { description = "Saturn"
               , planet      = Saturn
               , seconds     = 2000000000
               , expected    = 2.15
               }
        , Case { description = "Uranus"
               , planet      = Uranus
               , seconds     = 1210123456
               , expected    = 0.46
               }
        , Case { description = "Neptune"
               , planet      = Neptune
               , seconds     = 1821023456
               , expected    = 0.35
               }
        ]
