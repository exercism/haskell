{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ETL (transform)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "etl" $

  -- As of 2016-07-27, there was no reference file
  -- for the test cases in `exercism/x-common`.

  describe "transform" $ do

    it "transform one value" $
      transform (fromList [(1, "A")])
      `shouldBe` fromList [('a', 1)]

    it "transform multiple keys from one value" $
      transform (fromList [(1, "AE")])
      `shouldBe` fromList [('a', 1), ('e', 1)]

    it "transform multiple keys from multiple values" $
      transform (fromList [(1, "A"), (4, "B")])
      `shouldBe` fromList [('a', 1), ('b', 4)]

    it "full dataset" $
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
