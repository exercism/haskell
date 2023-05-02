import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import HelloWorld (hello)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = it "hello" $
          hello `shouldBe` "Hello, World!"
