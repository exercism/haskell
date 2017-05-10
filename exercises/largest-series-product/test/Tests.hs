{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Series (largestProduct)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "largestProduct" $ do

      it "finds the largest product if span equals length" $
        largestProduct 2 "29"
        `shouldBe` Just 18

      it "can find the largest product of 2 with numbers in order" $
        largestProduct 2 "0123456789"
        `shouldBe` Just 72

      it "can find the largest product of 2" $
        largestProduct 2 "576802143"
        `shouldBe` Just 48

      it "can find the largest product of 3 with numbers in order" $
        largestProduct 3 "0123456789"
        `shouldBe` Just 504

      it "can find the largest product of 3" $
        largestProduct 3 "1027839564"
        `shouldBe` Just 270

      it "can find the largest product of 5 with numbers in order" $
        largestProduct 5 "0123456789"
        `shouldBe` Just 15120

      it "can get the largest product of a big number" $
        largestProduct 6 "73167176531330624919225119674426574742355349194934"
        `shouldBe` Just 23520

      it "reports zero if the only digits are zero" $
        largestProduct 2 "0000"
        `shouldBe` Just 0

      it "reports zero if all spans include zero" $
        largestProduct 3 "99099"
        `shouldBe` Just 0

      it "rejects span longer than string length" $
        largestProduct 4 "123"
        `shouldBe` Nothing

      it "reports 1 for empty string and empty product (0 span)" $
        largestProduct 0 ""
        `shouldBe` Just 1

      it "reports 1 for nonempty string and empty product (0 span)" $
        largestProduct 0 "123"
        `shouldBe` Just 1

      it "rejects empty string and nonzero span" $
        largestProduct 1 ""
        `shouldBe` Nothing

      it "rejects invalid character in digits" $
        largestProduct 2 "1234a5"
        `shouldBe` Nothing

      it "rejects negative span" $
        largestProduct (-1) "12345"
        `shouldBe` Nothing
