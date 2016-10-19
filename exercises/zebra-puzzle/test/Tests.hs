import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import ZebraPuzzle (Resident(..), Solution(..), solve)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "zebra-puzzle" $
          it "solve" $ solve `shouldBe` Solution { waterDrinker = Norwegian
                                                 , zebraOwner   = Japanese  }

