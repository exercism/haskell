import Data.Char         (intToDigit)
import Test.Hspec        (Spec, describe, it)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck   (Positive(Positive), (==>), property)

import qualified Numeric as Num (showIntAtBase)

import Trinary (readTri, showTri)

{-
For the appropriate amount of challenge here, you should only
use functionality present in Prelude. Try not to use
Data.List, Data.Char, Data.Bits, or Numeric.

Try and use seq, $!, or BangPatterns appropriately to ensure
that the solution is efficient.

Handling invalid input is not necessary.

If you've done the Octal exercise, perhaps you should generalize it.
-}

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "trinary" $ do

    -- As of 2016-08-10, there was no reference file
    -- for the test cases in `exercism/x-common`.

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
