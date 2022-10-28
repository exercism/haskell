import GuessingGame (reply)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $
  describe "reply" $ do
    it "1 should be 'Too low'" $
      reply 1 `shouldBe` "Too low"
    it "100 should be 'Too high'" $
      reply 100 `shouldBe` "Too high"
    it "41 should be 'So close'" $
      reply 41 `shouldBe` "So close"
    it "43 should be 'So close'" $
      reply 43 `shouldBe` "So close"
    it "42 should be 'Correct'" $
      reply 42 `shouldBe` "Correct"
