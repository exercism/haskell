import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SecretHandshake (handshake)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "secret-handshake" $ do

    -- Test cases adapted from `exercism/x-common/secret-handshake` on 2017-01-31.

    it "wink for 1" $
      handshake (1 :: Int) `shouldBe` ["wink"]

    it "double blink for 10" $
      handshake (2 :: Int) `shouldBe` ["double blink"]

    it "close your eyes for 100" $
      handshake (4 :: Int) `shouldBe` ["close your eyes"]

    it "jump for 1000" $
      handshake (8 :: Int) `shouldBe` ["jump"]

    it "combine two actions" $
      handshake (3 :: Int) `shouldBe` ["wink", "double blink"]

    it "reverse two actions" $
      handshake (19 :: Int) `shouldBe` ["double blink", "wink"]

    it "reversing one action gives the same action" $
      handshake (24 :: Int) `shouldBe` ["jump"]

    it "reversing no actions still gives no actions" $
      handshake (16 :: Int) `shouldBe` []

    it "all possible actions" $
      handshake (15 :: Int) `shouldBe` ["wink", "double blink", "close your eyes", "jump"]

    it "reverse all possible actions" $
      handshake (31 :: Int) `shouldBe` ["jump", "close your eyes", "double blink", "wink"]

    it "do nothing for zero" $
      handshake (0 :: Int) `shouldBe` []

    it "do nothing if lower 5 bits not set" $
      handshake (32 :: Int) `shouldBe` []
