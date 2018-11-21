{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.QuickCheck   (Gen, forAll, forAllShrink, elements, sublistOf, suchThat)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List         (delete, lookup)
import Data.Maybe        (mapMaybe)

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

            -- Property: For an arbitrary `allergen` and its `score`,
            -- `isAllergicTo allergen score` is True.

            it "accepts single allergens" $ forAll allergenWithScore $
              uncurry isAllergicTo

            -- Property: For an arbitrary `score` and arbitrary `allergen`
            -- that does not match it, `isAllergicTo allergen score` is
            -- False.

            it "rejects mismatching allergens" $ forAll complementWithScore $
              not . uncurry isAllergicTo

            -- Property: For an arbitrary set of `allergens` and their
            -- combined `score`, it holds for all `allergen` in `allergens`
            -- that `isAllergicTo allergen score` is True.

            it "accepts multiple allergens" $ forAll allergensWithScore $
              \(allergens, score) -> all (`isAllergicTo` score) allergens

            -- Property: For an arbitrary `score` and all `allergens` that
            -- are not part of that score, `isAllergicTo allergen score` is
            -- False.

            it "rejects multiple mismatching allergens" $
              forAllShrink complementsWithScore shrinkComplementsWithScore $
                \(allergens, score) -> all (not . (`isAllergicTo` score)) allergens

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

            -- Property: For an arbitrary `allergen` and its `score`,
            -- `allergies score` is a list of exactly the element
            -- `allergen`.

            it "accepts single allergens" $ forAll allergenWithScore $
              \(allergen, score) -> allergies score == [allergen]

            -- Property: For an arbitrary set of `allergens` and their
            -- combined `score`, `allergies score` is a list of exactly
            -- `allergens`.

            it "accepts multiple allergens" $
              forAllShrink allergensWithScore shrinkAllergensWithScore $
                \(allergens, score) -> allergies score == allergens

allergenScores :: [(Allergen, Int)]
allergenScores =
  [ (Eggs,           1)
  , (Peanuts,        2)
  , (Shellfish,      4)
  , (Strawberries,   8)
  , (Tomatoes,      16)
  , (Chocolate,     32)
  , (Pollen,        64)
  , (Cats,         128)
  ]

allergenWithScore :: Gen (Allergen, Int)
allergenWithScore = elements allergenScores

complementWithScore :: Gen (Allergen, Int)
complementWithScore = do
  (allergen, score) <- allergenWithScore
  (complement, _) <- allergenWithScore `suchThat` ((/= allergen) . fst)
  return (complement, score)

allergensWithScore :: Gen ([Allergen], Int)
allergensWithScore = fmap sum . unzip <$> sublistOf allergenScores

complementsWithScore :: Gen ([Allergen], Int)
complementsWithScore = do
  (allergens, score) <- allergensWithScore
  let complements = [ allergen | (allergen, _) <- allergenScores
                               , allergen `notElem` allergens ]
  return (complements, score)

shrinkAllergensWithScore :: ([Allergen], Int) -> [([Allergen], Int)]
shrinkAllergensWithScore (allergens, score) =
  mapMaybe (\allergen -> without allergen <$> scoreFor allergen) allergens
  where
    without :: Allergen -> Int -> ([Allergen], Int)
    without allergen allergenScore =
      (delete allergen allergens, score - allergenScore)

    scoreFor :: Allergen -> Maybe Int
    scoreFor = flip lookup allergenScores

shrinkComplementsWithScore :: ([Allergen], Int) -> [([Allergen], Int)]
shrinkComplementsWithScore (allergens, score) = map without allergens
  where
    without :: Allergen -> ([Allergen], Int)
    without allergen = (delete allergen allergens, score)
