import Test.Hspec        (Spec, describe, it)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck   (Positive(Positive), property)

import qualified Numeric as Num (showOct)

import Octal (readOct, showOct)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "octal" $ do

    it "can show Int octal" $
      property $ \(Positive n) -> Num.showOct n "" == showOct (n :: Int)

    it "can show Integer octal" $
      property $ \(Positive n) -> Num.showOct n "" == showOct (n :: Integer)

    it "can read Int octal" $
      property $ \(Positive n) -> n == readOct (Num.showOct (n :: Int) "")

    it "can read Integer octal" $
      property $ \(Positive n) -> n == readOct (Num.showOct (n :: Integer) "")
