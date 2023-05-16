{-# LANGUAGE OverloadedStrings #-}

import Control.Monad     (foldM)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Forth (ForthError(..), emptyState, evalText, toList)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do

    let runTexts = fmap toList . foldM (flip evalText) emptyState

    describe "parsing and numbers" $
      it "numbers just get pushed onto the stack" $
        runTexts ["1 2 3 4 5"] `shouldBe` Right [1, 2, 3, 4, 5]

    describe "addition" $ do
      it "can add two numbers" $
        runTexts ["1 2 +"] `shouldBe` Right [3]
      it "errors if there is nothing on the stack" $
        runTexts ["+"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 +"] `shouldBe` Left StackUnderflow

    describe "subtraction" $ do
      it "can subtract two numbers" $
        runTexts ["3 4 -"] `shouldBe` Right [-1]
      it "errors if there is nothing on the stack" $
        runTexts ["-"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 -"] `shouldBe` Left StackUnderflow

    describe "multiplication" $ do
      it "can multiply two numbers" $
        runTexts ["2 4 *"] `shouldBe` Right [8]
      it "errors if there is nothing on the stack" $
        runTexts ["*"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 *"] `shouldBe` Left StackUnderflow

    describe "division" $ do
      it "can divide two numbers" $
        runTexts ["12 3 /"] `shouldBe` Right [4]
      it "performs integer division" $
        runTexts ["8 3 /"] `shouldBe` Right [2]
      it "errors if dividing by zero" $
        runTexts ["4 0 /"] `shouldBe` Left DivisionByZero
      it "errors if there is nothing on the stack" $
        runTexts ["/"] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 /"] `shouldBe` Left StackUnderflow

    describe "combined arithmetic" $ do
      it "addition and subtraction" $
        runTexts ["1 2 + 4 -"] `shouldBe` Right [-1]

      it "multiplication and division" $
        runTexts ["2 4 * 3 /"] `shouldBe` Right [2]

    describe "dup" $ do
      it "copies a value on the stack" $
        runTexts ["1 dup"  ] `shouldBe` Right [1, 1]
      it "copies the top value on the stack" $
        runTexts ["1 2 dup"] `shouldBe` Right [1, 2, 2]
      it "errors if there is nothing on the stack" $
        runTexts ["dup"    ] `shouldBe` Left StackUnderflow

    describe "drop" $ do
      it "removes the top value on the stack if it is the only one" $
        runTexts ["1 drop"  ] `shouldBe` Right []
      it "removes the top value on the stack if it is not the only one" $
        runTexts ["1 2 drop"] `shouldBe` Right [1]
      it "errors if there is nothing on the stack" $
        runTexts ["drop"    ] `shouldBe` Left StackUnderflow

    describe "swap" $ do
      it "swaps the top two values on the stack if they are the only ones" $
        runTexts ["1 2 swap"  ] `shouldBe` Right [2, 1]
      it "swaps the top two values on the stack if they are not the only ones" $
        runTexts ["1 2 3 swap"] `shouldBe` Right [1, 3, 2]
      it "errors if there is nothing on the stack" $
        runTexts ["swap"      ] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 swap"    ] `shouldBe` Left StackUnderflow

    describe "over" $ do
      it "copies the second element if there are only two" $
        runTexts ["1 2 over"  ] `shouldBe` Right [1, 2, 1]
      it "copies the second element if there are more than two" $
        runTexts ["1 2 3 over"] `shouldBe` Right [1, 2, 3, 2]
      it "errors if there is nothing on the stack" $
        runTexts ["over"      ] `shouldBe` Left StackUnderflow
      it "errors if there is only one value on the stack" $
        runTexts ["1 over"    ] `shouldBe` Left StackUnderflow

    describe "user-defined words" $ do
      it "can consist of built-in words" $
        runTexts [ ": dup-twice dup dup ;"
                 , "1 dup-twice"           ] `shouldBe` Right [1, 1, 1]

      it "execute in the right order" $
        runTexts [ ": countup 1 2 3 ;"
                 , "countup"           ] `shouldBe` Right [1, 2, 3]

      it "can override other user-defined words" $
        runTexts [ ": foo dup ;"
                 , ": foo dup dup ;"
                 , "1 foo"           ] `shouldBe` Right [1, 1, 1]

      it "can override built-in words" $
        runTexts [ ": swap dup ;"
                 , "1 swap"       ] `shouldBe` Right [1, 1]

      it "can override built-in operators" $
        runTexts [ ": + * ;"
                 , "3 4 +"   ] `shouldBe` Right [12]

      it "can use different words with the same name" $
        runTexts [ ": foo 5 ;"
                 , ": bar foo ;"
                 , ": foo 6 ;"
                 , "bar foo"     ] `shouldBe` Right [5, 6]

      it "can define word that uses word with the same name" $
        runTexts [ ": foo 10 ;"
                 , ": foo foo 1 + ;"
                 , "foo"             ] `shouldBe` Right [11]

      it "cannot redefine numbers" $
        runTexts [": 1 2 ;"] `shouldBe` Left InvalidWord

      it "errors if executing a non-existent word" $
        runTexts ["1 foo"] `shouldBe` Left (UnknownWord "foo")

      it "redefines an existing word with another existing word" $
        runTexts [ ": foo 5 ;"
                 , ": bar foo ;"
                 , ": foo 6 ;"
                 , ": bar foo ;"
                 , "bar foo"           ] `shouldBe` Right [6, 6]

    describe "case-insensitivity" $ do
      it "DUP is case-insensitive" $
        runTexts ["1 DUP Dup dup"         ] `shouldBe` Right [1, 1, 1, 1]
      it "DROP is case-insensitive" $
        runTexts ["1 2 3 4 DROP Drop drop"] `shouldBe` Right [1]
      it "SWAP is case-insensitive" $
        runTexts ["1 2 SWAP 3 Swap 4 swap"] `shouldBe` Right [2, 3, 4, 1]
      it "OVER is case-insensitive" $
        runTexts ["1 2 OVER Over over"    ] `shouldBe` Right [1, 2, 1, 2, 1]

      it "user-defined words are case-insensitive" $
        runTexts [ ": foo dup ;"
                 , "1 FOO Foo foo" ] `shouldBe` Right [1, 1, 1, 1]

      it "definitions are case-insensitive" $
        runTexts [ ": SWAP DUP Dup dup ;"
                 , "1 swap"               ] `shouldBe` Right [1, 1, 1, 1]

    describe "malformed word definitions" $ do  
      it "empty definition and no end" $
        runTexts [":"] `shouldBe` Left InvalidWord
      it "no end" $
        runTexts [": foo"] `shouldBe` Left InvalidWord
      it "no definition" $
        runTexts [": ;"] `shouldBe` Left InvalidWord

    describe "multiple definitions" $ do  
      it "on a line" $
        runTexts [": one 1 ; : two 2 ; one two +"] `shouldBe` Right [3]
      it "after ops" $
        runTexts ["1 2 + : addone 1 + ; addone"] `shouldBe` Right [4]
