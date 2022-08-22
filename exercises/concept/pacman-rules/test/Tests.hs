import PacmanRules (eatsGhost, loses, scores, wins)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "eatsGhost" $ do
    it "ghost eaten when touched while powered" $ do
      (eatsGhost True True) `shouldBe` True

    it "ghost not eaten when not touched" $ do
      (eatsGhost True False) `shouldBe` False

    it "ghost not eaten when touched un-powered" $ do
      (eatsGhost False True) `shouldBe` False

  describe "scores" $ do
    it "when eating dot" $ do
      (scores False True) `shouldBe` True

    it "when eating power pellet" $ do
      (scores True False) `shouldBe` True

    it "nothing when nothing eaten" $ do
      (scores False False) `shouldBe` False

  describe "loses" $ do
    it "if touching ghost un-powered" $ do
      (loses False True) `shouldBe` True

    it "is False if touching ghost while powered" $ do
      (loses True True) `shouldBe` False

    it "is False if not touching a ghost" $ do
      (loses True False) `shouldBe` False

  describe "wins" $ do
    it "if all dots are eaten" $ do
      (wins True False False) `shouldBe` True

    it "is False if all dots are eaten but touching a ghost" $ do
      (wins True False True) `shouldBe` False

    it "if all dots are eaten and touching a ghost while powered" $ do
      (wins True True True) `shouldBe` True
