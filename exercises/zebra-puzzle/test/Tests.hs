import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ZebraPuzzle (Resident(..), Solution(..), solve)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = it "solve" $ solve `shouldBe` Solution { waterDrinker = Norwegian
                                               , zebraOwner   = Japanese  }


-- 15994bece3c447e589ae7e66239e9ce55cac0e83
