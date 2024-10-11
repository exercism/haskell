import FreelancerRates (dailyRate, applyDiscount, monthlyRate, daysInBudget)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $
  describe "reply" $ do
    it "it's the hourly_rate times 8" $
      dailyRate 50 `shouldBe` 400.0

    it "it always returns a float" $
      dailyRate 60 `shouldBe` 480
    
    it "it does not round" $ 
      dailyRate 55.1 `shouldBe` 440.8

    it "a discount of 10% leaves 90% of the original price" $
      applyDiscount 140.0 10 `shouldBe` 126.0

    it "it always returns a double" $
      applyDiscount 100 10 `shouldBe` 90.0

    it "it doesn't round" $
      applyDiscount 111.11 13.5 `shouldBe` 96.11015

    it "it's the dailyRate times 22" $
      monthlyRate 62 0.0 `shouldBe` 10912

    it "it always returns an integer" $
      monthlyRate 70 0.0 `shouldBe` 12320

    it "the result is rounded up" $
      monthlyRate 62.8 0.0 `shouldBe` 11053

    it "the result should be rouneded up" $
      monthlyRate 65.2 0.0 `shouldBe` 11476

    it "gives a discount" $
      monthlyRate 67 12.0 `shouldBe` 10377

    it "the budget divided by the daily rate" $
      daysInBudget 1600 50 0.0 `shouldBe` 4
    
    it "always returns a float" $
      daysInBudget 520 65 0.0 `shouldBe` 1.0
    
    it "rounds down to one decimal place" $
      daysInBudget 4410 55 0.0 `shouldBe` 10.0
    
    it "rounds down down to one decimal" $
      daysInBudget 4480 55 0.0 `shouldBe` 10.1

    it "it applies the discount" $
      daysInBudget 480 60 20 `shouldBe` 1.2
    