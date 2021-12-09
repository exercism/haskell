import Test.Hspec             (it, shouldBe, hspec)
import LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

main :: IO ()
main = hspec $ do
    it "expectedMinutesInOven" $ do
        expectedMinutesInOven `shouldBe` 40

    it "preparationTimeInMinutes" $
        preparationTimeInMinutes 5 `shouldBe` 10

    it "elapsedTimeInMinutes" $ do
        elapsedTimeInMinutes 3 20 `shouldBe` 26
