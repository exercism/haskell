import Test.Hspec             (it, shouldBe, hspec)
import KitchenCalculator (getVolume, toMilliliter, fromMilliliter, convert, Messurments(..))

main :: IO ()
main = hspec $ do
    it "get cups" $ do
        getVolume (Cup, 1) `shouldBe` 1

    it "get fluid ounces" $ do
        getVolume (FluidOnce, 2) `shouldBe` 2

    it "get tablespoons" $ do
        getVolume (TableSpoon, 3) `shouldBe` 3
    
    it "get teaspoons" $ do
        getVolume (TeaSpoon, 4) `shouldBe` 4
    
    it "get milliliters" $ do
        getVolume (Milliliter, 5) `shouldBe` 5

    it "convert milliliters to milliliters" $ do
        toMilliliter (Milliliter, 3) `shouldBe` (Milliliter, 3)

    it "convert cups to milliliters" $ do
        toMilliliter (Cup, 3) `shouldBe` (Milliliter, 720)

    it "convert fluid ounces to milliliters" $ do
        toMilliliter (FluidOnce, 100) `shouldBe` (Milliliter, 3000)

    it "convert tablespoons to milliliters" $ do
        toMilliliter (TableSpoon, 3) `shouldBe` (Milliliter, 45)

    it "convert teaspoons to milliliters" $ do
        toMilliliter (TeaSpoon, 3) `shouldBe` (Milliliter, 15)

    it "convert from milliliters to milliliters" $ do
        fromMilliliter (Milliliter, 4) Milliliter `shouldBe` (Milliliter, 4)
    
    it "convert from milliliters to cups" $ do
        fromMilliliter (Milliliter, 840) Cup `shouldBe` (Cup, 3.5)
    
    it "convert from milliliters to fluid ounces" $ do
        fromMilliliter (Milliliter, 4522.5) FluidOnce `shouldBe` (FluidOnce, 150.75)

    it "convert from milliliters to tablespoons" $ do
        fromMilliliter (Milliliter, 71.25) TableSpoon `shouldBe` (TableSpoon, 4.75)

    it "convert from milliliters to teaspoons" $ do
        fromMilliliter (Milliliter, 61.25) TeaSpoon `shouldBe` (TeaSpoon, 12.25)

    it "teaspoon to tablespoon" $ do
        convert (TeaSpoon, 15) TableSpoon `shouldBe` (TableSpoon, 5)

    it "cups to fluid ounces" $ do
        convert (Cup, 4) FluidOnce `shouldBe` (FluidOnce, 32)

    it "fluid ounces to teaspoons" $ do
        convert (FluidOnce, 4) TeaSpoon `shouldBe` (TeaSpoon, 24)

    it "tablespoons to cups" $ do
        convert (TableSpoon, 320) Cup `shouldBe` (Cup, 20)
