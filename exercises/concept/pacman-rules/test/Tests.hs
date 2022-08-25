import PacmanRules (eatsGhost, loses, scores, wins)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "eatsGhost" $ do
    it "ghost eaten when touched while powered" $ do
      eatsGhost True True `shouldBe` True

    it "ghost not eaten when not touched" $ do
      eatsGhost True False `shouldBe` False

    it "ghost not eaten when touched un-powered" $ do
      eatsGhost False True `shouldBe` False

    it "ghost not eaten when not touched while un-powered" $ do
      eatsGhost False False `shouldBe` False

  describe "scores" $ do
    it "when eating dot" $ do
      scores False True `shouldBe` True

    it "when eating power pellet" $ do
      scores True False `shouldBe` True

    it "nothing when nothing eaten" $ do
      scores False False `shouldBe` False

    it "when eating power pellet and dot" $ do
      scores True True `shouldBe` True

  describe "loses" $ do
    it "if touching ghost un-powered " $ do
      loses False True `shouldBe` True

    it "is False if touching ghost powered" $ do
      loses True True `shouldBe` False

    it "is False if not touching a ghost powered" $ do
      loses True False `shouldBe` False

    it "is False if not touching a ghost un-powered" $ do
      loses False False `shouldBe` False

  describe "wins" $ do
    it "if has eaten all dots and not touching a ghost un-powered" $ do
      wins True False False `shouldBe` True

    it "if has eaten all dots and touching a ghost powered" $ do
      wins True True True `shouldBe` True

    it "if has eaten all dots and not touching ghost powered" $ do
      wins True True False `shouldBe` True

    it "is False if has eaten all dots and touching ghost un-powered" $ do
      wins True False True `shouldBe` False

    it "is False if has not eaten all dots and not touching a ghost un-powered" $ do
      wins False False False `shouldBe` False

    it "is False if has not eaten all dots and touching a ghost powered" $ do
      wins False True True `shouldBe` False

    it "is False if has not eaten all dots and not touching ghost powered" $ do
      wins False True False `shouldBe` False

    it "is False if has not eaten all dots and touching ghost un-powered" $ do
      wins False False True `shouldBe` False
