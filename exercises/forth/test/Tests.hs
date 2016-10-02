{-# LANGUAGE OverloadedStrings #-}

import Control.Monad     (foldM)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Forth (ForthError(..), empty, evalText, formatStack)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "forth" $ do

    -- As of 2016-10-02, there was no reference file
    -- for the test cases in `exercism/x-common`.

    let runTexts = fmap formatStack . foldM (flip evalText) empty

    it "no input, no stack" $
      formatStack empty `shouldBe` ""

    it "numbers just get pushed onto the stack" $
      runTexts ["1 2 3 4 5"] `shouldBe` Right "1 2 3 4 5"

    it "non-word characters are separators" $
      runTexts ["1\NUL2\SOH3\n4\r5 6\t7"] `shouldBe` Right "1 2 3 4 5 6 7"

    it "basic arithmetic" $ do
      runTexts ["1 2 + 4 -"] `shouldBe` Right "-1"
      runTexts ["2 4 * 3 /"] `shouldBe` Right "2"

    it "division by zero" $
      runTexts ["4 2 2 - /"] `shouldBe` Left DivisionByZero

    it "dup" $ do
      runTexts ["1 DUP"  ] `shouldBe` Right "1 1"
      runTexts ["1 2 Dup"] `shouldBe` Right "1 2 2"
      runTexts ["dup"    ] `shouldBe` Left StackUnderflow

    it "drop" $ do
      runTexts ["1 drop"  ] `shouldBe` Right ""
      runTexts ["1 2 drop"] `shouldBe` Right "1"
      runTexts ["drop"    ] `shouldBe` Left StackUnderflow

    it "swap" $ do
      runTexts ["1 2 swap"  ] `shouldBe` Right "2 1"
      runTexts ["1 2 3 swap"] `shouldBe` Right "1 3 2"
      runTexts ["1 swap"    ] `shouldBe` Left StackUnderflow
      runTexts ["swap"      ] `shouldBe` Left StackUnderflow

    it "over" $ do
      runTexts ["1 2 over"  ] `shouldBe` Right "1 2 1"
      runTexts ["1 2 3 over"] `shouldBe` Right "1 2 3 2"
      runTexts ["1 over"    ] `shouldBe` Left StackUnderflow
      runTexts ["over"      ] `shouldBe` Left StackUnderflow

    it "defining a new word" $
      runTexts [ ": dup-twice dup dup ;"
               , "1 dup-twice"           ] `shouldBe` Right "1 1 1"

    it "redefining an existing word" $
      runTexts [ ": foo dup ;"
               , ": foo dup dup ;"
               , "1 foo"           ] `shouldBe` Right "1 1 1"

    it "redefining an existing built-in word" $
      runTexts [ ": swap dup ;"
               , "1 swap"       ] `shouldBe` Right "1 1"

    it "defining words with odd characters" $
      runTexts [": € 220371 ; €"] `shouldBe` Right "220371"

    it "defining a number" $
      runTexts [": 1 2 ;"] `shouldBe` Left InvalidWord

    it "calling a non-existing word" $
      runTexts ["1 foo"] `shouldBe` Left (UnknownWord "foo")
