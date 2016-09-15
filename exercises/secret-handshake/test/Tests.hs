import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SecretHandshake (handshake)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "secret-handshake" $ do

    -- As of 2016-09-12, there was no reference file
    -- for the test cases in `exercism/x-common`.

    it "1 to wink" $ do
      handshake (1 :: Int) `shouldBe` ["wink"]
      handshake "1"        `shouldBe` ["wink"]

    it "10 to double blink" $ do
      handshake (2 :: Int) `shouldBe` ["double blink"]
      handshake "10"       `shouldBe` ["double blink"]

    it "100 to close your eyes" $ do
      handshake (4 :: Int) `shouldBe` ["close your eyes"]
      handshake "100"      `shouldBe` ["close your eyes"]

    it "1000 to jump" $ do
      handshake (8 :: Int) `shouldBe` ["jump"]
      handshake "1000"     `shouldBe` ["jump"]

    it "11 to wink and double blink" $ do
      handshake (3 :: Int) `shouldBe` ["wink", "double blink"]
      handshake "11"       `shouldBe` ["wink", "double blink"]

    it "10011 to double blink and wink" $ do
      handshake (19 :: Int) `shouldBe` ["double blink", "wink"]
      handshake "10011"     `shouldBe` ["double blink", "wink"]

    it "11111 to jump, close your eyes, double blink, and wink" $ do
      handshake (31 :: Int) `shouldBe` ["jump", "close your eyes", "double blink", "wink"]
      handshake "11111"     `shouldBe` ["jump", "close your eyes", "double blink", "wink"]

    it "zero" $ do
      handshake (0 :: Int) `shouldBe` []
      handshake "0"        `shouldBe` []

    it "gibberish" $
      handshake "piggies" `shouldBe` []

    it "partial gibberish" $
      handshake "1piggies" `shouldBe` []
