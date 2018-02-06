import Control.Arrow     ((&&&))
import Test.Hspec        (Spec, it, shouldBe, shouldNotBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import qualified Data.Vector as Vector (fromList)

import Matrix
  ( Matrix
  , cols
  , column
  , flatten
  , fromList
  , fromString
  , reshape
  , row
  , rows
  , shape
  , transpose
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let intMatrix = fromString :: String -> Matrix Int
    let vector = Vector.fromList

    it "extract row from one number matrix" $
      row 0 (intMatrix "1") `shouldBe` vector [1]

    it "can extract row" $
      row 1 (intMatrix "1 2\n3 4") `shouldBe` vector [3, 4]

    it "extract row where numbers have different widths" $
      row 1 (intMatrix "1 2\n10 20") `shouldBe` vector [10, 20]

    it "can extract row from non-square matrix" $
      row 2 (intMatrix "1 2 3\n4 5 6\n7 8 9\n8 7 6") `shouldBe` vector [7, 8, 9]

    it "extract column from one number matrix" $
      column 0 (intMatrix "1") `shouldBe` vector [1]

    it "can extract column" $
      column 2 (intMatrix "1 2 3\n4 5 6\n7 8 9") `shouldBe` vector [3, 6, 9]

    it "can extract column from non-square matrix" $
      column 2 (intMatrix "1 2 3\n4 5 6\n7 8 9\n8 7 6") `shouldBe` vector [3, 6, 9, 6]

    it "extract column where numbers have different widths" $
      column 1 (intMatrix "89 1903 3\n18 3 1\n9 4 800") `shouldBe` vector [1903, 3, 4]

    -- Track-specific tests:

    it "shape" $ do
      shape (intMatrix ""        ) `shouldBe` (0, 0)
      shape (intMatrix "1"       ) `shouldBe` (1, 1)
      shape (intMatrix "1\n2"    ) `shouldBe` (2, 1)
      shape (intMatrix "1 2"     ) `shouldBe` (1, 2)
      shape (intMatrix "1 2\n3 4") `shouldBe` (2, 2)

    it "rows & cols" $
      (rows &&& cols) (intMatrix "1 2") `shouldBe` (1, 2)

    it "eq" $ do

      intMatrix "1 2" `shouldBe`    intMatrix "1 2"
      intMatrix "2 3" `shouldNotBe` intMatrix "1 2 3"

    it "fromList" $ do
      fromList [[1 ,  2]] `shouldBe` intMatrix "1 2"
      fromList [[1], [2]] `shouldBe` intMatrix "1\n2"

    it "transpose" $ do
      transpose (intMatrix "1\n2\n3"      ) `shouldBe` intMatrix "1 2 3"
      transpose (intMatrix "1 4\n2 5\n3 6") `shouldBe` intMatrix "1 2 3\n4 5 6"

    it "reshape" $
      reshape (2, 2) (intMatrix "1 2 3 4") `shouldBe` intMatrix "1 2\n3 4"

    it "flatten" $
      flatten (intMatrix "1 2\n3 4") `shouldBe` vector [1, 2, 3, 4]
