import Test.Hspec             (it, shouldBe, hspec)
import LuciansLusciousLasagna (totalTimeInMinutes, remainingMinutesInOven, expectedMinutesInOven, preparationTimeInMinutes, alarm)

main :: IO ()
main = hspec $ do
    it "expected minutes in oven" $ do
        expectedMinutesInOven `shouldBe` 40

    it "remaining minutes in oven"  $
        remainingMinutesInOven 25 `shouldBe` 15

    it "remaining minutes in oven, a few minutes later" $ 
        remainingMinutesInOven 30 `shouldBe` 10

    it  "preparation time in minutes for one layer" $
        preparationTimeInMinutes 1 `shouldBe` 2

    it "preparation time in minutes for multiple layers" $
        preparationTimeInMinutes 4 `shouldBe` 8

    it "total time in minutes for one layer" $
        totalTimeInMinutes 1 30 `shouldBe` 32

    it "total time in minutes for multiple layers" $
        totalTimeInMinutes 4 8 `shouldBe` 16

    it "notification message" $
        alarm `shouldBe` "DING!"
    