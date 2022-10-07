import Data.Char         (toUpper)
import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Accumulate (accumulate)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let square x = x * x :: Int

    it "empty accumulation" $
      accumulate square []
      `shouldBe` []

    it "accumulate squares" $
      accumulate square [1, 2, 3]
      `shouldBe` [1, 4, 9]

    it "accumulate upcases" $
      accumulate (map toUpper) ["hello", "world"]
      `shouldBe` ["HELLO", "WORLD"]

    it "accumulate reversed strings" $
      accumulate reverse ["the", "quick", "brown", "fox", "etc"]
      `shouldBe` ["eht", "kciuq", "nworb", "xof", "cte"]

    it "accumulate recursively" $
      accumulate (\c -> accumulate ((c:) . show) ([1, 2, 3] :: [Int])) "abc"
      `shouldBe` [["a1", "a2", "a3"], ["b1", "b2", "b3"], ["c1", "c2", "c3"]]

    it "accumulate non-strict" $
      take 1 (accumulate id ("nice work!" : error "accumulate should be even lazier, don't use reverse!"))
      `shouldBe` ["nice work!"]
