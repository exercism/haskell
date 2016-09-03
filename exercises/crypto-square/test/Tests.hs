import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import CryptoSquare
  ( ciphertext
  , normalizeCiphertext 
  , normalizePlaintext
  , plaintextSegments
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "crypto-square" $ do

    -- Test cases adapted from `exercism/x-common/crypto-square.json`
    -- on 2016-08-02. Some deviations exist and are noted in comments.

    describe "normalizePlaintext" $ do

      it "Lowercase" $
          normalizePlaintext "Hello"
          `shouldBe`         "hello"

      it "Remove spaces" $
          normalizePlaintext "Hi there"
          `shouldBe`         "hithere"

      it "Remove punctuation" $
          normalizePlaintext "@1, 2%, 3 Go!"
          `shouldBe`         "123go"

    describe "plaintextSegments" $ do

      it "empty plaintext results in an empty rectangle" $
          plaintextSegments ""
          `shouldBe`        []

      it "4 character plaintext results in an 2x2 rectangle" $
          plaintextSegments "Ab Cd"
          `shouldBe`        [ "ab"
                            , "cd" ]

      it "9 character plaintext results in an 3x3 rectangle" $
          plaintextSegments "This is fun!"
          `shouldBe`        [ "thi"
                            , "sis"
                            , "fun" ]

      it "54 character plaintext results in an 8x7 rectangle" $
          plaintextSegments "If man was meant to stay on the ground, god would have given us roots."
          `shouldBe`        [ "ifmanwas"
                            , "meanttos"
                            , "tayonthe"
                            , "groundgo"
                            , "dwouldha"
                            , "vegivenu"
                            , "sroots"  ]

    describe "ciphertext" $ do

    -- The function described by the reference file in `x-common`
    -- as `encoded` is called `ciphertext` in this track.

      it "empty plaintext results in an empty encode" $
          ciphertext ""
          `shouldBe` ""

      it "Non-empty plaintext results in the combined plaintext segments" $
          ciphertext "If man was meant to stay on the ground, god would have given us roots."
          `shouldBe` "imtgdvsfearwermayoogoanouuiontnnlvtwttddesaohghnsseoau"

    describe "normalizeCiphertext" $ do

    -- The function described by the reference file in `x-common`
    -- as `ciphertext` is called `normalizeCiphertext` in this track.

      it "empty plaintext results in an empty ciphertext" $
          normalizeCiphertext ""
          `shouldBe`          ""

      it "9 character plaintext results in 3 chunks of 3 characters" $
          normalizeCiphertext "This is fun!"
          `shouldBe`          "tsf hiu isn"

    {- In this track the encoded text chunks are not padded with spaces.

      it "54 character plaintext results in 7 chunks, the last two padded with spaces" $
          normalizeCiphertext "If man was meant to stay on the ground, god would have given us roots."
          `shouldBe`          "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "

    -}
