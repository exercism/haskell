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
        let testPal slDesc result expPal expFac =
              describe slDesc $ case result of
                                Just (value, factors) -> do
                                  it "value"   $ value             `shouldBe` expPal
                                  it "factors" $ normalize factors `shouldBe` expFac
                                Nothing ->
                                  it "result"  $ Nothing           `shouldBe` Just (expPal, [expFac])
        testPal "smallestPalindrome" (smallestPalindrome minFactor maxFactor) sPal sPalFactors
        testPal "largestPalindrome" (largestPalindrome minFactor maxFactor) lPal lPalFactors
    test (desc, minFactor, maxFactor, Nothing) =
      describe desc $ do
        describe "smallestPalindrome" $
          it "result" $ smallestPalindrome minFactor maxFactor `shouldBe` Nothing
        describe "largestPalindrome" $
          it "result" $ largestPalindrome minFactor maxFactor  `shouldBe` Nothing

    cases = [ ("palindromes from single digit factors",     1,     9,         Just (1, [(    1,     1)],          9, [(1, 9), (3, 3)]))
            , ("palindromes from double digit factors",    10,    99,       Just (121, [(   11,    11)],       9009, [(   91,    99)]))
            , ("palindromes from triple digit factors",   100,   999,     Just (10201, [(  101,   101)],     906609, [(  913,   993)]))
            , ("palindromes from four digit factors"  ,  1000,  9999,   Just (1002001, [( 1001,  1001)],   99000099, [( 9901,  9999)]))
            , ("no available palindrome"              ,  1002,  1003,                                                          Nothing)
            , ("invalid range"                        , 10000,     1,                                                          Nothing) ]
