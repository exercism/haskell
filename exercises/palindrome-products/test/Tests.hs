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
    test (desc, minFactor, maxFactor, Just (sPal, sPalFactors, lPal, lPalFactors)) =
      describe desc $ do
        let sortPair (a, b)  = if a < b then (a, b) else (b, a)
        let normalize        = sort . nub . map sortPair
        describe "smallestPalindrome" $
          case smallestPalindrome minFactor maxFactor of
            Just (value, factors) -> do
              it "value"   $ value             `shouldBe` sPal
              it "factors" $ normalize factors `shouldBe` sPalFactors
            Nothing -> do
              it "result"  $ Nothing           `shouldBe` Just (sPal, [sPalFactors])
        describe "largestPalindrome" $
          case largestPalindrome minFactor maxFactor of
            Just (value, factors) -> do
              it "value"   $ value             `shouldBe` lPal
              it "factors" $ normalize factors `shouldBe` lPalFactors
            Nothing -> do
              it "result" $ Nothing            `shouldBe` Just (lPal, [lPalFactors])
    test (desc, minFactor, maxFactor, Nothing) =
      describe desc $ do
        describe "smallestPalindrome" $ do
          let result = smallestPalindrome minFactor maxFactor
          it "result"   $ result             `shouldBe` Nothing
        describe "largestPalindrome" $ do
          let result = largestPalindrome minFactor maxFactor
          it "result"   $ result             `shouldBe` Nothing

    cases = [ ("palindromes from single digit factors",     1,     9,         Just (1, [(    1,     1)],          9, [(1, 9), (3, 3)]))
            , ("palindromes from double digit factors",    10,    99,       Just (121, [(   11,    11)],       9009, [(   91,    99)]))
            , ("palindromes from triple digit factors",   100,   999,     Just (10201, [(  101,   101)],     906609, [(  913,   993)]))
            , ("palindromes from four digit factors"  ,  1000,  9999,   Just (1002001, [( 1001,  1001)],   99000099, [( 9901,  9999)]))
            , ("no available palindrome"              ,  1002,  1003,                                                          Nothing)
            , ("invalid range"                        , 10000,     1,                                                          Nothing) ]
