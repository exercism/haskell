{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (for_)
import EliudsEggs (eggCount)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "eggCount" $ for_ cases test
  where
    test Case {..} = it description assertion
      where
        assertion = expression `shouldBe` output
        expression = eggCount input

data Case = Case
  { description :: String,
    input :: Int,
    output :: Int
  }

cases :: [Case]
cases =
  [ Case
      { description = "0 eggs",
        input = 0,
        output = 0
      },
    Case
      { description = "1 egg",
        input = 16,
        output = 1
      },
    Case
      { description = "4 eggs",
        input = 89,
        output = 4
      },
    Case
      { description = "13 eggs",
        input = 2000000000,
        output = 13
      }
  ]
