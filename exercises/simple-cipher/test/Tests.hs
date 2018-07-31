import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Cipher (caesarDecode, caesarEncode, caesarEncodeRandom)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    it "no-op encode" $ do
      caesarEncode         "a"  ['a'..'z'] `shouldBe` ['a'..'z']
      caesarEncode (repeat 'a') ['a'..'z'] `shouldBe` ['a'..'z']

    it "no-op decode" $ do
      caesarDecode         "a"  ['a'..'z'] `shouldBe` ['a'..'z']
      caesarDecode (repeat 'a') ['a'..'z'] `shouldBe` ['a'..'z']

    it "reversible" $ do
      let k0 = "alkjsdhflkjahsuid"
          k1 = ['z','y'..'a']
      caesarDecode k0 (caesarEncode k0 "asdf") `shouldBe` "asdf"
      caesarDecode k1 (caesarEncode k1 "asdf") `shouldBe` "asdf"

    it "known cipher" $ do
      let k = ['a'..'j']
          encode = caesarEncode k
          decode = caesarDecode k
      encode "aaaaaaaaaa"        `shouldBe` k
      decode k                   `shouldBe` "aaaaaaaaaa"
      decode (encode ['a'..'z']) `shouldBe` ['a'..'z']
      encode "zzzzzzzzzz"        `shouldBe` 'z':['a'..'i']
      decode ('z':['a'..'i'])    `shouldBe` "zzzzzzzzzz"

    it "double shift" $ do
      let plaintext  = "iamapandabear"
          ciphertext = "qayaeaagaciai"
      caesarEncode plaintext plaintext `shouldBe` ciphertext

    it "shift cipher" $ do
      let encode = caesarEncode "d"
          decode = caesarDecode "d"
      encode "aaaaaaaaaa"        `shouldBe` "dddddddddd"
      decode "dddddddddd"        `shouldBe` "aaaaaaaaaa"
      encode ['a'..'j']          `shouldBe` ['d'..'m']
      decode (encode ['a'..'j']) `shouldBe` ['a'..'j']

    it "random tests" $ do
      let plaintext = take 1000 (cycle ['a'..'z'])
      p1 <- caesarEncodeRandom plaintext
      uncurry caesarDecode p1 `shouldBe` plaintext
      p2 <- caesarEncodeRandom plaintext
      uncurry caesarDecode p2 `shouldBe` plaintext
      -- There's a small chance this could fail, since it's random.
      (p1 == p2) `shouldBe` False
