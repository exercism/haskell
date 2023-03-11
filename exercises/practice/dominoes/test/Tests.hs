{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad     (forM_, unless)
import Data.Foldable     (for_)
import Data.Function     (on)
import Test.Hspec        (Spec, describe, expectationFailure, it, shouldBe, shouldMatchList)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Text.Printf       (printf)

import Dominoes (chain)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "chain" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion = if expected
                      then shouldBeChain $ chain input
                      else chain input `shouldBe` Nothing

        shouldBeChain Nothing = expectationFailure "should have had a chain, but didn't"
        shouldBeChain (Just output) = do
          output `shouldMatchDominoesOf` input
          consecutivesShouldMatch output
          endsShouldMatch output

        shouldMatchDominoesOf = shouldMatchList `on` map sortDomino

        consecutivesShouldMatch l = forM_ (indexedPairs l) $ \(d1, d2, i) ->
          shouldBeConsecutive l ("domino " ++ show i, d1) ("domino " ++ show (i + 1), d2)

        endsShouldMatch [] = return ()
        endsShouldMatch l@(d1:_) =
          shouldBeConsecutive l (" last domino", last l) ("first domino", d1)

        indexedPairs :: [a] -> [(a, a, Int)]
        indexedPairs l = zip3 l (drop 1 l) [1..]

        sortDomino (a, b) = if a > b then (b, a) else (a, b)

        shouldBeConsecutive l (name1, d1@(_, d1r)) (name2, d2@(d2l, _)) =
          unless (d1r == d2l) $
            expectationFailure $
              printf "In chain %s:\n\t   right end of %s (%s)\n\tand left end of %s (%s)\n\tdidn't match!" (show l) name1 (show d1) name2 (show d2)

data Case = Case { description :: String
                 , input       :: [(Int, Int)]
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "empty input = empty output"
               , input       = []
               , expected    = True
               }
        , Case { description = "singleton input = singleton output"
               , input       = [(1, 1)]
               , expected    = True
               }
        , Case { description = "singleton that can't be chained"
               , input       = [(1, 2)]
               , expected    = False
               }
        , Case { description = "three elements"
               , input       = [(1, 2), (3, 1), (2, 3)]
               , expected    = True
               }
        , Case { description = "can reverse dominoes"
               , input       = [(1, 2), (1, 3), (2, 3)]
               , expected    = True
               }
        , Case { description = "cannot use the same domino in both directions"
               , input       = [(1, 2), (2, 3), (2, 1)]
               , expected    = False
               }
        , Case { description = "can't be chained"
               , input       = [(1, 2), (4, 1), (2, 3)]
               , expected    = False
               }
        , Case { description = "disconnected - simple"
               , input       = [(1, 1), (2, 2)]
               , expected    = False
               }
        , Case { description = "disconnected - double loop"
               , input       = [(1, 2), (2, 1), (3, 4), (4, 3)]
               , expected    = False
               }
        , Case { description = "disconnected - single isolated"
               , input       = [(1, 2), (2, 3), (3, 1), (4, 4)]
               , expected    = False
               }
        , Case { description = "need backtrack"
               , input       = [(1, 2), (2, 3), (3, 1), (2, 4), (2, 4)]
               , expected    = True
               }
        , Case { description = "separate loops"
               , input       = [(1, 2), (2, 3), (3, 1), (1, 1), (2, 2), (3, 3)]
               , expected    = True
               }
        , Case { description = "nine elements"
               , input       = [(1, 2), (5, 3), (3, 1), (1, 2), (2, 4), (1, 6), (2, 3), (3, 4), (5, 6)]
               , expected    = True
               }
        , Case { description = "twelve elements - no loop"
               , input       = [(1, 2), (5, 3), (3, 1), (1, 2), (2, 4), (1, 6), (2, 3), (3, 4), (5, 6), (3,6), (4,5), (2,1)]
               , expected    = False
               }
        ]
