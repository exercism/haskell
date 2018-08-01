{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Queens (boardString, canAttack)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    -- Track-specific test cases.

    describe "boardString" $ do

      it "empty board" $ boardString Nothing Nothing
        `shouldBe` unlines [ "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _" ]

      it "board with just white queen" $ boardString (Just (2, 4)) Nothing
        `shouldBe` unlines [ "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ W _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _" ]

      it "board with just black queen" $ boardString Nothing (Just (0, 0))
        `shouldBe` unlines [ "B _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _" ]

      it "board" $ boardString (Just (2, 4)) (Just (6, 6))
        `shouldBe` unlines [ "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ W _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ _ _"
                           , "_ _ _ _ _ _ B _"
                           , "_ _ _ _ _ _ _ _" ]

    -- The function described by the reference file as `create` doesn't
    -- exist in this track, so only the `canAttack` test cases were
    -- implemented here

    describe "canAttack" $ do

      let test (description, white, black, expected) =
            it description $ canAttack white black `shouldBe` expected

          cases = [ ("can not attack"               , (2, 4), (6, 6), False)
                  , ("can attack on same rank"      , (2, 4), (2, 6), True )
                  , ("can attack on same file"      , (4, 5), (2, 5), True )
                  , ("can attack on first diagonal" , (2, 2), (0, 4), True )
                  , ("can attack on second diagonal", (2, 2), (3, 1), True )
                  , ("can attack on third diagonal" , (2, 2), (1, 1), True )
                  , ("can attack on fourth diagonal", (2, 2), (5, 5), True ) ]

      for_ cases test
