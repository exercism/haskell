{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import School (add, empty, grade, sorted)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

          let fromList = foldr (uncurry add) empty
          let fromGrade g = fromList . zip (repeat g)

          it "add student" $
            sorted (add 2 "Aimee" empty) `shouldBe` [(2, ["Aimee"])]

          it "add more students in same class" $
            sorted (fromGrade 2 ["James", "Blair", "Paul"])
            `shouldBe` [(2, ["Blair", "James", "Paul"])]

          it "add students to different grades" $
            sorted (fromList [(3, "Chelsea"), (7, "Logan")])
            `shouldBe` [(3, ["Chelsea"]), (7, ["Logan"])]

          it "empty list if no students" $
            sorted empty `shouldBe` []

          it "get students in a grade" $
            grade 5 (fromList [(5, "Franklin"), (5, "Bradley"), (1, "Jeff")])
            `shouldBe` ["Bradley", "Franklin"]

          it "get students in a non-existent grade" $
            grade 1 empty `shouldBe` []

          it "sorted school" $
            sorted (fromList [ (4, "Jennifer"   )
                             , (6, "Kareem"     )
                             , (4, "Christopher")
                             , (3, "Kyle"       ) ] )
            `shouldBe` [ (3, ["Kyle"                   ] )
                       , (4, ["Christopher", "Jennifer"] )
                       , (6, ["Kareem"                 ] ) ]
