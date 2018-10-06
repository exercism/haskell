{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad       (replicateM, unless)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (lift)
import Data.Ix             (inRange)
import Data.List           (group, intercalate, null, sort)
import Test.Hspec          (Spec, expectationFailure, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.Hspec.Runner   (configFastFail, defaultConfig, hspecWith)

import Robot (initialState, mkRobot, resetName, robotName)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

          let a = ('A', 'Z')
          let d = ('0', '9')
          let matchesPattern s = length s == 5
                                 && and (zipWith inRange [a, a, d, d, d] s)
          let testPersistence r = do
                n1 <- robotName r
                n2 <- robotName r
                n3 <- robotName r
                n1 `shouldBe` n2
                n1 `shouldBe` n3
          let evalWithInitial = flip evalStateT initialState

          it "name should match expected pattern" $
            evalWithInitial mkRobot >>= robotName >>= (`shouldSatisfy` matchesPattern)

          it "name is persistent" $
            evalWithInitial mkRobot >>= testPersistence

          it "different robots have different names" $
            evalWithInitial $ do
              robots <- replicateM 5000 mkRobot
              names <- traverse (lift . robotName) robots
              let repeats = map head . filter ((>1) . length) . group . sort $ names
              lift $ unless (null repeats) $
                expectationFailure $ "Repeat name(s) found: " ++ intercalate ", " repeats

          it "new name should match expected pattern" $
            evalWithInitial $ do
              r <- mkRobot
              resetName r
              lift $ robotName r >>= (`shouldSatisfy` matchesPattern)

          it "new name is persistent" $
            evalWithInitial $ do
              r <- mkRobot
              resetName r >> lift (testPersistence r)

          it "new name is different from old name" $
            evalWithInitial $ do
              r <- mkRobot
              n1 <- lift $ robotName r
              resetName r
              n2 <- lift $ robotName r
              lift $ n1 `shouldNotBe` n2

          it "resetting a robot affects only one robot" $
            evalWithInitial $ do
              r1 <- mkRobot
              r2 <- mkRobot
              n1 <- lift $ robotName r1
              n2 <- lift $ robotName r2
              lift $ n1 `shouldNotBe` n2
              resetName r1
              n1' <- lift $ robotName r1
              n2' <- lift $ robotName r2
              lift $ n1' `shouldNotBe` n2'
              lift $ n2  `shouldBe`    n2'
