{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import GameOfLife (tick)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "tick" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = tick matrix `shouldBe` expected

data Case = Case { description :: String
                 , matrix      :: [[Int]]
                 , expected    :: [[Int]]
                 }

cases :: [Case]
cases = [ Case { description = "Empty matrix"
               , matrix      = []
               , expected    = []
               }
        , Case { description = "Live cells with zero live neighbors die"
               , matrix      = [
                                [0, 0, 0],
                                [0, 1, 0],
                                [0, 0, 0]
                               ]
               , expected    = [
                                [0, 0, 0],
                                [0, 0, 0],
                                [0, 0, 0]
                               ]
               }
        , Case { description = "Live cells with only one live neighbor die"
               , matrix      = [
                                [0, 0, 0],
                                [0, 1, 0],
                                [0, 1, 0]
                               ]
               , expected    = [
                                [0, 0, 0],
                                [0, 0, 0],
                                [0, 0, 0]
                               ]
               }
        , Case { description = "Live cells with two live neighbors stay alive"
               , matrix      = [
                                [1, 0, 1],
                                [1, 0, 1],
                                [1, 0, 1]
                               ]
               , expected    = [
                                [0, 0, 0],
                                [1, 0, 1],
                                [0, 0, 0]
                               ]
               }
        , Case { description = "Live cells with three live neighbors stay alive"
               , matrix      = [
                                [0, 1, 0],
                                [1, 0, 0],
                                [1, 1, 0]
                               ]
               , expected    = [
                                [0, 0, 0],
                                [1, 0, 0],
                                [1, 1, 0]
                               ]
               }
        , Case { description = "Dead cells with three live neighbors become alive"
               , matrix      = [
                                [1, 1, 0],
                                [0, 0, 0],
                                [1, 0, 0]
                               ]
               , expected    = [
                                [0, 0, 0],
                                [1, 1, 0],
                                [0, 0, 0]
                               ]
               }
        , Case { description = "Live cells with four or more neighbors die"
               , matrix      = [
                                [1, 1, 1],
                                [1, 1, 1],
                                [1, 1, 1]
                               ]
               , expected    = [
                                [1, 0, 1],
                                [0, 0, 0],
                                [1, 0, 1]
                               ]
               }
        , Case { description = "Bigger matrix"
               , matrix      = [
                                [1, 1, 0, 1, 1, 0, 0, 0],
                                [1, 0, 1, 1, 0, 0, 0, 0],
                                [1, 1, 1, 0, 0, 1, 1, 1],
                                [0, 0, 0, 0, 0, 1, 1, 0],
                                [1, 0, 0, 0, 1, 1, 0, 0],
                                [1, 1, 0, 0, 0, 1, 1, 1],
                                [0, 0, 1, 0, 1, 0, 0, 1],
                                [1, 0, 0, 0, 0, 0, 1, 1]
                               ]
               , expected    = [
                                [1, 1, 0, 1, 1, 0, 0, 0],
                                [0, 0, 0, 0, 0, 1, 1, 0],
                                [1, 0, 1, 1, 1, 1, 0, 1],
                                [1, 0, 0, 0, 0, 0, 0, 1],
                                [1, 1, 0, 0, 1, 0, 0, 1],
                                [1, 1, 0, 1, 0, 0, 0, 1],
                                [1, 0, 0, 0, 0, 0, 0, 0],
                                [0, 0, 0, 0, 0, 0, 1, 1]
                               ]
               }
        ]
