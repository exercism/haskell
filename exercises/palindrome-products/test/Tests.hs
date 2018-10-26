{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Data.List         (nub, sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Palindromes (largestPalindrome, smallestPalindrome)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = for_ cases test
  where
    test (desc, minFactor, maxFactor, sPal, sPalFactors, lPal, lPalFactors) =
      describe desc $ do
        let sortPair (a, b)  = if a < b then (a, b) else (b, a)
        let normalize        = sort . nub . map sortPair
        describe "smallestPalindrome" $ do
          let (value, factors) = smallestPalindrome minFactor maxFactor
          it "value"   $ value             `shouldBe` sPal
          it "factors" $ normalize factors `shouldBe` sPalFactors
        describe "largestPalindrome" $ do
          let (value, factors) = largestPalindrome minFactor maxFactor
          it "value"   $ value             `shouldBe` lPal
          it "factors" $ normalize factors `shouldBe` lPalFactors

    cases = [ ("palindromes from single digit factors",     1,     9,         1, [(    1,     1)],          9, [(1, 9), (3, 3)])
            , ("palindromes from double digit factors",    10,    99,       121, [(   11,    11)],       9009, [(   91,    99)])
            , ("palindromes from triple digit factors",   100,   999,     10201, [(  101,   101)],     906609, [(  913,   993)])
            , ("palindromes from four digit factors"  ,  1000,  9999,   1002001, [( 1001,  1001)],   99000099, [( 9901,  9999)])
            , ("no available smallest palindrome"     ,  1002,  1003,         0,               [],          0,               [])
            , ("no available largest palindrome"      ,    15,    15,         0,               [],          0,               [])
            , ("invalid range"                        , 10000,     1,         0,               [],          0,               []) ]
