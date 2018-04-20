import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Garden
  ( Plant ( Clover
          , Grass
          , Radishes
          , Violets
          )
  , garden
  , lookupPlants
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

defaultStudents :: [String]
defaultStudents =
  [ "Alice", "Bob", "Charlie", "David", "Eve", "Fred"
  , "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
  ]

specs :: Spec
specs = do

    it "garden with single student" $ do

      let alicePlants = lookupPlants "Alice" . garden defaultStudents

      alicePlants "RC\nGG" `shouldBe` [ Radishes, Clover, Grass   , Grass  ]
      alicePlants "VC\nRC" `shouldBe` [ Violets , Clover, Radishes, Clover ]

    it "garden with two students" $ do

      let plants s  = lookupPlants s $ garden defaultStudents plantList
          plantList = "VVCG\nVVRC"

      plants "Bob" `shouldBe` [ Clover, Grass, Radishes, Clover ]

    it "garden with three students" $ do

      let plants s  = lookupPlants s $ garden defaultStudents plantList
          plantList = "VVCCGG\nVVCCGG"

      plants "Bob"     `shouldBe` [ Clover, Clover, Clover, Clover ]
      plants "Charlie" `shouldBe` [ Grass , Grass , Grass , Grass  ]

    it "full garden" $ do

      let plants s  = lookupPlants s $ garden defaultStudents plantList
          plantList = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"

      plants "Alice"   `shouldBe` [ Violets , Radishes, Violets , Radishes ]
      plants "Bob"     `shouldBe` [ Clover  , Grass   , Clover  , Clover   ]
      plants "Kincaid" `shouldBe` [ Grass   , Clover  , Clover  , Grass    ]
      plants "Larry"   `shouldBe` [ Grass   , Violets , Clover  , Violets  ]
