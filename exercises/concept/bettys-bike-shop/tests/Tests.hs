import           BettysBikeShop (penceToPounds, poundsToString)
import           Test.Hspec     (hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    it "599 pence should be 5.99 pounds" $ do
        penceToPounds 599 `shouldBe` 5.99

    it "33 pence should be 0.33 pounds" $ do
        penceToPounds 33 `shouldBe` 0.33

    it "5.99 pounds should be formatted as £5.99" $ do
        poundsToString 5.99 `shouldBe` "£5.99"

    it "0.33 pounds should be formatted as £0.33" $ do
        poundsToString 0.33 `shouldBe` "£0.33"
