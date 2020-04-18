{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# language LambdaCase #-}

import Data.Either       (isLeft)
import Data.Map          (findWithDefault)
import Test.Hspec        (Spec, describe, it, shouldSatisfy)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import DNA (nucleotideCounts, Nucleotide(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

          let x `matchesMap` y = shouldSatisfy x $ \case
                Right count -> and [ findWithDefault 0 n count == c | (n,c) <- y ]
                Left _ -> False

          describe "nucleotideCounts" $ do

            it "empty dna strand has no nucleotides" $
              nucleotideCounts "" `matchesMap` [ (A, 0)
                                               , (C, 0)
                                               , (G, 0)
                                               , (T, 0) ]

            it "can count one nucleotide in single-character input" $
              nucleotideCounts "G" `matchesMap` [ (A, 0)
                                                , (C, 0)
                                                , (G, 1)
                                                , (T, 0) ]

            it "repetitive-sequence-has-only-guanosine" $
              nucleotideCounts "GGGGGGGG" `matchesMap` [ (A, 0)
                                                       , (C, 0)
                                                       , (G, 8)
                                                       , (T, 0) ]

            it "counts all nucleotides" $
              nucleotideCounts "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
              `matchesMap` [ (A, 20)
                           , (C, 12)
                           , (G, 17)
                           , (T, 21) ]

            it "validates strand" $
              nucleotideCounts "AGXXACT" `shouldSatisfy` isLeft
