{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Darts (score)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "darts" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = score x y `shouldBe` expected

data Case = Case { description :: String
                 , x           :: Float
                 , y           :: Float
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "Missed target"
               , x           = -9
               , y           = 9
               , expected    = 0
               }
        , Case { description = "On the outer circle"
               , x           = 0
               , y           = 10
               , expected    = 1
               }
        , Case { description = "On the middle circle"
               , x           = -5
               , y           = 0
               , expected    = 5
               }
        , Case { description = "On the inner circle"
               , x           = 0
               , y           = -1
               , expected    = 10
               }
        , Case { description = "Exactly on center"
               , x           = 0
               , y           = 0
               , expected    = 10
               }
        , Case { description = "Near the center"
               , x           = -0.1
               , y           = -0.1
               , expected    = 10
               }
        , Case { description = "Just within the inner circle"
               , x           = 0.7
               , y           = 0.7
               , expected    = 10
               }
        , Case { description = "Just outside the inner circle"
               , x           = 0.8
               , y           = -0.8
               , expected    = 5
               }
        , Case { description = "Just within the middle circle"
               , x           = -3.5
               , y           = 3.5
               , expected    = 5
               }
        , Case { description = "Just outside the middle circle"
               , x           = -3.6
               , y           = -3.6
               , expected    = 1
               }
        , Case { description = "Just within the outer circle"
               , x           = -7
               , y           = -7
               , expected    = 1
               }
        , Case { description = "Just outside the outer circle"
               , x           = 7.1
               , y           = -7.1
               , expected    = 0
               }
        , Case { description = "Asymmetric position between the inner and middle circles"
               , x           = 0.5
               , y           = -4
               , expected    = 5
               }
        ]
