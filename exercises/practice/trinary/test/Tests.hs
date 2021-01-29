import Data.Char         (intToDigit)
import Test.Hspec        (Spec, it)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck   (Positive(Positive), (==>), property)

import qualified Numeric as Num (showIntAtBase)

import Trinary (readTri, showTri)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let refShowTri n = Num.showIntAtBase 3 intToDigit n ""

    it "can show Int trinary" $
      property $ \(Positive n) -> refShowTri n == showTri (n :: Int)

    it "can show Integer trinary" $
      property $ \(Positive n) -> refShowTri n == showTri (n :: Integer)

    it "can read Int trinary" $
      property $ \(Positive n) -> n == readTri (refShowTri (n :: Int))

    it "can read Integer trinary" $
      property $ \(Positive n) -> n == readTri (refShowTri (n :: Integer))

    it "can read invalid trinary" $
      \n -> any (`notElem` ['0'..'2']) (show n) ==> (readTri . show $ n) == (0 :: Int)
