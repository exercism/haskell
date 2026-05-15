import GuessingGame (reply)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
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
