import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import BST
  ( bstLeft
  , bstRight
  , bstValue
  , empty
  , fromList
  , insert
  , singleton
  , toList
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let int4   = 4  ::  Int
    let noInts = [] :: [Int]

    it "data is retained" $
      bstValue (singleton int4) `shouldBe` Just 4

    it "inserting less" $ do
      let t = insert 2 (singleton int4)
      bstValue t `shouldBe` Just 4
      (bstLeft t >>= bstValue) `shouldBe` Just 2

    it "inserting same" $ do
      let t = insert 4 (singleton int4)
      bstValue t `shouldBe` Just 4
      (bstLeft t >>= bstValue) `shouldBe` Just 4

    it "inserting right" $ do
      let t = insert 5 (singleton int4)
      bstValue t `shouldBe` Just 4
      (bstRight t >>= bstValue) `shouldBe` Just 5

    it "empty list to tree" $
      fromList noInts `shouldBe` empty

    it "empty list has no value" $
      bstValue (fromList noInts) `shouldBe` Nothing

    it "inserting into empty" $ do
      let t = insert int4 empty
      bstValue t `shouldBe` Just 4

    it "complex tree" $ do
      let t = fromList [int4, 2, 6, 1, 3, 7, 5]
      bstValue  t                            `shouldBe` Just 4
      (bstLeft  t >>= bstValue             ) `shouldBe` Just 2
      (bstLeft  t >>= bstLeft  >>= bstValue) `shouldBe` Just 1
      (bstLeft  t >>= bstRight >>= bstValue) `shouldBe` Just 3
      (bstRight t >>= bstValue             ) `shouldBe` Just 6
      (bstRight t >>= bstLeft  >>= bstValue) `shouldBe` Just 5
      (bstRight t >>= bstRight >>= bstValue) `shouldBe` Just 7

    it "empty tree to list" $
      length (toList empty) `shouldBe` 0

    it "iterating one element" $
      toList (singleton int4) `shouldBe` [4]

    it "iterating over smaller element" $
      toList (fromList [int4, 2]) `shouldBe` [2, 4]

    it "iterating over equal element" $
      toList (fromList [int4, 4]) `shouldBe` [4, 4]

    it "iterating over larger element" $
      toList (fromList [int4, 5]) `shouldBe` [4, 5]

    it "iterating over complex tree" $
      toList (fromList [int4, 2, 1, 3, 6, 7, 5]) `shouldBe` [1..7]
