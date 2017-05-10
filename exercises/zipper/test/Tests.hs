import Data.Maybe        (fromJust)
import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let leaf v     = node v Nothing Nothing
        node v l r = Just (BT v l r :: BinTree Int)
        t1         = BT 1 (node 2 Nothing  $ leaf 3) $ leaf 4
        t2         = BT 1 (node 5 Nothing  $ leaf 3) $ leaf 4
        t3         = BT 1 (node 2 (leaf 5) $ leaf 3) $ leaf 4
        t4         = BT 1 (leaf 2                  ) $ leaf 4

    it "data is retained" $
      toTree (fromTree t1)
      `shouldBe` t1

    it "left, right and value" $
      (value . fromJust . right . fromJust . left . fromTree) t1
      `shouldBe` 3

    it "dead end" $
      (left . fromJust . left . fromTree) t1
      `shouldBe` Nothing

    it "tree from deep focus" $
      (toTree . fromJust . right . fromJust . left . fromTree) t1
      `shouldBe` t1

    it "setValue" $
      (toTree . setValue 5 . fromJust . left . fromTree) t1
      `shouldBe` t2

    it "setLeft with Just" $
      (toTree . setLeft (leaf 5) . fromJust . left . fromTree) t1
      `shouldBe` t3

    it "setRight with Nothing" $
      (toTree . setRight Nothing . fromJust . left . fromTree) t1
      `shouldBe` t4

    it "different paths to same zipper" $
      (right . fromJust . up . fromJust . left . fromTree) t1
      `shouldBe` (right . fromTree) t1
