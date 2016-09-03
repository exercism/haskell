import Test.Hspec        (Spec, describe, it)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck   (Positive(Positive), property)

import qualified Numeric as Num (showOct)

import Octal (readOct, showOct)

{-
For the appropriate amount of challenge here, you should only
use functionality present in Prelude. Try not to use
Data.List, Data.Char, Data.Bits, or Numeric.

Try and use seq, $!, or BangPatterns appropriately to ensure
that the solution is efficient.

Handling invalid input is not necessary.
-}

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "octal" $ do

    -- As of 2016-08-10, there was no reference file
    -- for the test cases in `exercism/x-common`.

    it "can show Int octal" $
      property $ \(Positive n) -> Num.showOct n "" == showOct (n :: Int)

    it "can show Integer octal" $
      property $ \(Positive n) -> Num.showOct n "" == showOct (n :: Integer)

    it "can read Int octal" $
      property $ \(Positive n) -> n == readOct (Num.showOct (n :: Int) "")

    it "can read Integer octal" $
      property $ \(Positive n) -> n == readOct (Num.showOct (n :: Integer) "")
