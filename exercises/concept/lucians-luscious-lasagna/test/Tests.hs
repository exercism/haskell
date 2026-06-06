import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
    it "expectedMinutesInOven" $ do
        expectedMinutesInOven `shouldBe` 40

    it "preparationTimeInMinutes" $
        preparationTimeInMinutes 5 `shouldBe` 10

    it "elapsedTimeInMinutes" $ do
        elapsedTimeInMinutes 3 20 `shouldBe` 26
