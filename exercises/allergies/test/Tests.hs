{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Allergies
  ( Allergen ( Cats
             , Chocolate
             , Eggs
             , Peanuts
             , Pollen
             , Shellfish
             , Strawberries
             , Tomatoes
             )
  , allergies
  , isAllergicTo
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

          describe "isAllergicTo" $ do

            it "no allergies means not allergic" $ do
              let score = 0
              isAllergicTo Peanuts      score `shouldBe` False
              isAllergicTo Cats         score `shouldBe` False
              isAllergicTo Strawberries score `shouldBe` False

            it "is allergic to eggs" $ do
              let score = 1
              isAllergicTo Eggs         score `shouldBe` True

            it "allergic to eggs in addition to other stuff" $ do
              let score = 5
              isAllergicTo Eggs         score `shouldBe` True
              isAllergicTo Shellfish    score `shouldBe` True
              isAllergicTo Strawberries score `shouldBe` False

            it "allergic to strawberries but not peanuts" $ do
              let score = 9
              isAllergicTo Eggs         score `shouldBe` True
              isAllergicTo Peanuts      score `shouldBe` False
              isAllergicTo Shellfish    score `shouldBe` False
              isAllergicTo Strawberries score `shouldBe` True

          describe "allergies" $ do

            let xs `shouldMatch` ys =  all (`elem` ys) xs
                                    && all (`elem` xs) ys

            it "no allergies at all" $
              allergies   0 `shouldMatch` []

            it "allergic to just eggs" $
              allergies   1 `shouldMatch` [ Eggs ]

            it "allergic to just peanuts" $
              allergies   2 `shouldMatch` [ Peanuts ]

            it "allergic to just strawberries" $
              allergies   8 `shouldMatch` [ Strawberries ]

            it "allergic to eggs and peanuts" $
              allergies   3 `shouldMatch` [ Eggs
                                          , Peanuts ]

            it "allergic to more than eggs but not peanuts" $
              allergies   5 `shouldMatch` [ Eggs
                                          , Shellfish ]

            it "allergic to lots of stuff" $
              allergies 248 `shouldMatch` [ Cats
                                          , Chocolate
                                          , Pollen
                                          , Strawberries
                                          , Tomatoes     ]

            it "allergic to everything" $
              allergies 255 `shouldMatch` [ Cats
                                          , Chocolate
                                          , Eggs
                                          , Peanuts
                                          , Pollen
                                          , Shellfish
                                          , Strawberries
                                          , Tomatoes     ]

            it "ignore non allergen score parts" $
              allergies 509 `shouldMatch` [ Cats
                                          , Chocolate
                                          , Eggs
                                          , Pollen
                                          , Shellfish
                                          , Strawberries
                                          , Tomatoes     ]
