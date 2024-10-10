import GuessingGame (reply)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
  describe "reply" $ do
    it "correct when the guessed number equals secret" $ do
      reply 7 (Guess 7) `shouldBe` "Correct"

    it "too high when guessed number is greater than the secret" $ do
      reply 9 (Guess 18) `shouldBe` "Too high"

    it "too low when guessed number is less than the secret" $ do
      reply 42 (Guess 30) `shouldBe` "Too low"

    it "so close when guess differs from secret by -1" $ do
      reply 64 63 `shouldBe` "So close!"

    it "so close when guess differs from secret by +1" $ do
      reply 52 53 `shouldBe` "So close!"

    it "when no guess is supplied, ask the player to make a guess" $ do
      reply 42 NoGuess `shouldBe` "Please make a guess"
