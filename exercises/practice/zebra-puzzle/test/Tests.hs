import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import ZebraPuzzle (Resident(..), Solution(..), solve)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = it "solve" $ solve `shouldBe` Solution { waterDrinker = Norwegian
                                               , zebraOwner   = Japanese  }
