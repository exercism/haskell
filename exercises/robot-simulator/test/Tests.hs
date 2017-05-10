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
  , simulate
  , turnLeft
  , turnRight
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

    -- The reference tests for `turnLeft` and `turnRight` describe
    -- functions that are applied to robots positioned at (0, 0).
    -- In this track, they are functions over directions, so those
    -- test cases cannot be completely implemented.

    describe "turnRight" $ do

      it "turn from North" $ turnRight North `shouldBe` East
      it "turn from East"  $ turnRight East  `shouldBe` South
      it "turn from South" $ turnRight South `shouldBe` West
      it "turn from West"  $ turnRight West  `shouldBe` North

    describe "turnLeft" $ do

      it "turn from North" $ turnLeft North `shouldBe` West
      it "turn from West"  $ turnLeft West  `shouldBe` South
      it "turn from South" $ turnLeft South `shouldBe` East
      it "turn from East"  $ turnLeft East  `shouldBe` North

    describe "simulate advance" $ do

    -- The function described by the reference file as `advance`
    -- doesn't exist in this track, so we test `simulate` with "A".

      let dir `from` pos = simulate (mkRobot dir pos) "A"

      it "does not change the direction" $
        bearing (North `from` (0, 0)) `shouldBe` North

      it "increases the y coordinate one when facing north" $
        coordinates (North `from` (0, 0)) `shouldBe` (0, 1)

      it "decreases the y coordinate by one when facing south" $
        coordinates (South `from` (0, 0)) `shouldBe` (0, -1)

      it "increases the x coordinate by one when facing east" $
        coordinates (East `from` (0, 0)) `shouldBe` (1, 0)

      it "decreases the x coordinate by one when facing west" $
        coordinates (West `from` (0, 0)) `shouldBe `(-1, 0)

    describe "simulate" $ do

    -- The function described by the reference file as
    -- `instructions` is called `simulate` in this track.

      let simulation pos dir = simulate (mkRobot dir pos)

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
