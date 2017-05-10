{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ETL (transform)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs =

  describe "transform" $ do

    it "a single letter" $
      transform (fromList [(1, "A")])
      `shouldBe` fromList [('a', 1)]

    it "single score with multiple letters" $
      transform (fromList [(1, "AEIOU")])
      `shouldBe` fromList [('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1)]

    it "multiple scores with multiple letters" $
      transform (fromList [(1, "AE"), (2, "DG")])
      `shouldBe` fromList [('a', 1), ('e', 1), ('d', 2), ('g', 2)]

    it "multiple scores with differing numbers of letters" $
      transform (fromList fullInput)
      `shouldBe` fromList fullOutput

  where

    fullInput = [ ( 1, "AEIOULNRST")
                , ( 2, "DG"        )
                , ( 3, "BCMP"      )
                , ( 4, "FHVWY"     )
                , ( 5, "K"         )
                , ( 8, "JX"        )
                , (10, "QZ"        ) ]

    fullOutput = [ ('a',  1) , ('b',  3) , ('c',  3) , ('d',  2)
                 , ('e',  1) , ('f',  4) , ('g',  2) , ('h',  4)
                 , ('i',  1) , ('j',  8) , ('k',  5) , ('l',  1)
                 , ('m',  3) , ('n',  1) , ('o',  1) , ('p',  3)
                 , ('q', 10) , ('r',  1) , ('s',  1) , ('t',  1)
                 , ('u',  1) , ('v',  4) , ('w',  4) , ('x',  8)
                 , ('y',  4) , ('z', 10) ]
