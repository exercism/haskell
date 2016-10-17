{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ZebraPuzzle (Resident(..), Solution(..), solve)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "zebra-puzzle" $
          describe "solve" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion  = expression `shouldBe` solution
        expression = solve

data Case = Case { description :: String
                 , solution    :: Solution
                 }

cases :: [Case]
cases = [ Case { description  = "solution"
               , solution    = Solution { waterDrinker = Norwegian
                                        , zebraOwner = Japanese
                                        }
               }
        ]
