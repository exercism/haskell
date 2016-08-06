{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Luhn (addends, checkDigit, checksum, create, isValid)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "luhn" $ do
    describe "standard tests" $ do

      -- Test cases adapted from `exercism/x-common/luhn.json` on
      -- 2016-08-04. Some deviations exist and are noted in comments.

      it "check digit" $
        checkDigit 34567 `shouldBe` 7

      it "check digit with input ending in zero" $
        checkDigit 91370 `shouldBe` 0

      it "check addends" $
        addends 12121 `shouldBe` [1, 4, 1, 4, 1]

      it "check too large addends" $
        addends 8631 `shouldBe` [7, 6, 6, 1]

      -- The reference test cases expect the checksum function to return
      -- the simple sum of the transformed digits, not their `mod 10` sum.
      -- In this track, we insist on the `mod 10`. :)

      it "checksum" $
        checksum 4913 `shouldBe` 2      -- The reference test expects 22.

      it "checksum of larger number" $
        checksum 201773 `shouldBe` 1    -- The reference test expects 21.

      it "check invalid number" $
        isValid 738 `shouldBe` False

      it "check valid number" $
        isValid 8739567 `shouldBe` True

      it "create valid number" $
        create 123 `shouldBe` 1230

      it "create larger valid number" $
        create 873956 `shouldBe` 8739567

      it "create even larger valid number" $
        create 837263756 `shouldBe` 8372637564

    describe "track-specific tests" $ do

      -- This track has some tests that were not included in the
      -- reference test cases from `exercism/x-common/leap.json`.

      it "checksum 1111" $
        checksum 1111 `shouldBe` 6

      it "checksum 8763" $
        checksum 8763 `shouldBe` 0

      it "checksum 8739567" $
        checksum 8739567 `shouldBe` 0

      it "checksum 2323200577663554" $
        checksum 2323200577663554 `shouldBe` 0

      it "isValid 1111" $
        isValid 1111 `shouldBe` False

      it "isValid 8763" $
        isValid 8763 `shouldBe` True

      it "isValid 2323200577663554" $
        isValid 2323200577663554 `shouldBe` True

      it "create 232320057766355" $
        create 232320057766355 `shouldBe` 2323200577663554
