{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Robot
  ( Bearing ( East
            , North
            , South
            , West
            )
  , bearing
  , coordinates
  , mkRobot
  , move
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    describe "mkRobot" $ do

    -- The function described by the reference file
    -- as `create` is called `mkRobot` in this track.

      it "A robot is created with a position and a direction" $ do
        let robot = mkRobot North (0, 0)
        coordinates robot `shouldBe` (0, 0)
        bearing     robot `shouldBe` North

      it "Negative positions are allowed" $ do
        let robot = mkRobot South (-1, -1)
        coordinates robot `shouldBe` (-1, -1)
        bearing     robot `shouldBe` South

    let turnTest inst dir dir2 =
          let robot = mkRobot dir (0, 0) in
          describe ("from " ++ show dir) $ do
            it "should change direction" $
              bearing (move robot inst) `shouldBe` dir2
            it "shouldn't change position" $
              coordinates (move robot inst) `shouldBe` (0, 0)

    describe "turning right rotates the robot's direction 90 degrees clockwise" $ do
      let rightTest = turnTest "R"
      rightTest North East
      rightTest East South
      rightTest South West
      rightTest West North

    describe "turning left rotates the robot's direction 90 degrees counter-clockwise" $ do
      let leftTest = turnTest "L"
      leftTest North West
      leftTest West South
      leftTest South East
      leftTest East North

    describe "advancing" $ do
      let dir `from` pos = move (mkRobot dir pos) "A"
      let test desc dir pos =
            describe (show dir ++ " from " ++ show pos) $ do
              it "shouldn't change direction" $
                bearing (dir `from` (0, 0)) `shouldBe` dir
              it desc $
                coordinates (dir `from` (0, 0)) `shouldBe` pos

      test "increases the y coordinate one when facing north" North (0, 1)
      test "decreases the y coordinate by one when facing south" South (0, -1)
      test "increases the x coordinate by one when facing east" East (1, 0)
      test "decreases the x coordinate by one when facing west" West (-1, 0)

    describe "move" $ do

      let simulation pos dir = move (mkRobot dir pos)

      it "instructions to move east and north from README" $ do
        let robot = simulation (7, 3) North "RAALAL"
        coordinates robot `shouldBe` (9, 4)
        bearing     robot `shouldBe` West

      it "instructions to move west and north" $ do
        let robot = simulation (0, 0) North "LAAARALA"
        coordinates robot `shouldBe` (-4, 1)
        bearing     robot `shouldBe` West

      it "instructions to move west and south" $ do
        let robot = simulation (2, -7) East "RRAAAAALA"
        coordinates robot `shouldBe` (-3, -8)
        bearing     robot `shouldBe` South

      it "instructions to move east and north" $ do
        let robot = simulation (8, 4) South "LAAARRRALLLL"
        coordinates robot `shouldBe` (11, 5)
        bearing     robot `shouldBe` North
