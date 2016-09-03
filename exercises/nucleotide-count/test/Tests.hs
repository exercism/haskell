{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Either       (isLeft)
import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import DNA (count, nucleotideCounts)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "nucleotide-count" $ do

          -- As of 2016-07-27, there was no reference file
          -- for the test cases in `exercism/x-common`.

          let x `matches`    y = x `shouldBe`  Right y
          let x `matchesMap` y = x `shouldBe` (Right . fromList) y

          describe "count" $ do

            it "empty dna strand has no adenosine" $
              count 'A' "" `matches` 0

            it "repetitive cytidine gets counted" $
              count 'C' "CCCCC" `matches` 5

            it "counts only thymidine" $
              count 'T' "GGGGGTAACCCGG" `matches` 1

            it "validates nucleotides" $
              count 'X' "GACT" `shouldSatisfy` isLeft

            it "validates strand" $
              count 'G' "GACYT" `shouldSatisfy` isLeft

          describe "nucleotideCounts" $ do

            it "empty dna strand has no nucleotides" $
              nucleotideCounts "" `matchesMap` [ ('A', 0)
                                               , ('C', 0)
                                               , ('G', 0)
                                               , ('T', 0) ]

            it "repetitive-sequence-has-only-guanosine" $
              nucleotideCounts "GGGGGGGG" `matchesMap` [ ('A', 0)
                                                       , ('C', 0)
                                                       , ('G', 8)
                                                       , ('T', 0) ]

            it "counts all nucleotides" $
              nucleotideCounts "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
              `matchesMap` [ ('A', 20)
                           , ('C', 12)
                           , ('G', 17)
                           , ('T', 21) ]

            it "validates strand" $
              nucleotideCounts "GPAC" `shouldSatisfy` isLeft
