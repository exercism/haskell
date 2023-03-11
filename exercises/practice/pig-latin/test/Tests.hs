{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import PigLatin (translate)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "translate" $ do

    describe "ay is added to words that start with vowels" $ do
        it "word beginning with a" $ translate "apple"  `shouldBe` "appleay"
        it "word beginning with e" $ translate "ear"    `shouldBe` "earay"
        it "word beginning with i" $ translate "igloo"  `shouldBe` "iglooay"
        it "word beginning with o" $ translate "object" `shouldBe` "objectay"
        it "word beginning with u" $ translate "under"  `shouldBe` "underay"

    describe "first letter and ay are moved to the end of words that start with consonants" $ do
        it "word beginning with p" $ translate "pig"    `shouldBe` "igpay"
        it "word beginning with k" $ translate "koala"  `shouldBe` "oalakay"
        it "word beginning with y" $ translate "yellow" `shouldBe` "ellowyay"
        it "word beginning with x" $ translate "xenon"  `shouldBe` "enonxay"
        it "word beginning with q without a following u" $ translate "qat" `shouldBe` "atqay"

    describe "some letter clusters are treated like a single consonant" $ do
        it "word beginning with ch"  $ translate "chair"   `shouldBe` "airchay"
        it "word beginning with qu"  $ translate "queen"   `shouldBe` "eenquay"
        it "word beginning with qu and a preceding consonant" $ translate "square" `shouldBe` "aresquay"
        it "... but not words beginning with a vowel then qu" $ translate "equal"  `shouldBe` "equalay"
        it "word beginning with th"  $ translate "therapy" `shouldBe` "erapythay"
        it "word beginning with thr" $ translate "thrush"  `shouldBe` "ushthray"
        it "word beginning with sch" $ translate "school"  `shouldBe` "oolschay"

    describe "some letter clusters are treated like a single vowel" $ do
        it "word beginning with yt" $ translate "yttria" `shouldBe` "yttriaay"
        it "word beginning with xr" $ translate "xray"   `shouldBe` "xrayay"

    describe "position of y in a word determines if it is a consonant or a vowel" $ do
        it "y is treated like a consonant at the beginning of a word" $
          translate "yellow" `shouldBe` "ellowyay"
        it "y is treated like a vowel at the end of a consonant cluster" $
          translate "rhythm" `shouldBe` "ythmrhay"
        it "y as second letter in two letter word" $
          translate "my" `shouldBe` "ymay"

    describe "phrases are translated" $
        it "a whole phrase" $ translate "quick fast run" `shouldBe` "ickquay astfay unray"
