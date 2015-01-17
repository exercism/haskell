import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import System.Exit (ExitCode(..), exitWith)
import Trinary (showTri, readTri)
import qualified Numeric as N
import Data.Char (intToDigit)

{-
For the appropriate amount of challenge here, you should only
use functionality present in Prelude. Try not to use
Data.List, Data.Char, Data.Bits, or Numeric.

Try and use seq, $!, or BangPatterns appropriately to ensure
that the solution is efficient.

Handling invalid input is not necessary.

If you've done the Octal exercise, perhaps you should generalize it.
-}

exitProperly :: IO Bool -> IO ()
exitProperly m = do
  didSucceed <- m
  exitWith $ if didSucceed then ExitSuccess else ExitFailure 1

refShowTri :: (Integral a, Show a) => a -> String
refShowTri n = N.showIntAtBase 3 intToDigit n ""

prop_showTri_integral :: (Integral a, Show a) => Positive a -> Property
prop_showTri_integral (Positive n) = property $ refShowTri n == showTri n

prop_showTri_int :: Positive Int -> Property
prop_showTri_int = prop_showTri_integral

prop_readTri_integral :: (Integral a, Show a) => Positive a -> Property
prop_readTri_integral (Positive n) = property $ n == readTri (refShowTri n)

prop_readTri_int :: Positive Int -> Property
prop_readTri_int = prop_readTri_integral

prop_readInvalidTri_just_digits :: Positive Int -> Property
prop_readInvalidTri_just_digits n = any (`notElem` ['0'..'2']) (show n) ==> (readTri . show $ n) == (0 :: Int)

main :: IO ()
main = exitProperly $ all isSuccess `fmap` mapM quickCheckResult
  [ prop_showTri_integral
  , prop_showTri_int
  , prop_readTri_integral
  , prop_readTri_int
  , prop_readInvalidTri_just_digits
  ]
