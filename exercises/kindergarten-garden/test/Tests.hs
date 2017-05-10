import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Garden
  ( Plant ( Clover
          , Grass
          , Radishes
          , Violets
          )
  , defaultGarden
  , garden
  , lookupPlants
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    it "garden with single student" $ do

      let alicePlants = lookupPlants "Alice" . defaultGarden

      alicePlants "RC\nGG" `shouldBe` [ Radishes, Clover, Grass   , Grass  ]
      alicePlants "VC\nRC" `shouldBe` [ Violets , Clover, Radishes, Clover ]

    it "garden with two students" $ do

      let plants s  = lookupPlants s $ defaultGarden plantList
          plantList = "VVCG\nVVRC"

      plants "Bob" `shouldBe` [ Clover, Grass, Radishes, Clover ]

    it "garden with three students" $ do

      let plants s  = lookupPlants s $ defaultGarden plantList
          plantList = "VVCCGG\nVVCCGG"

      plants "Bob"     `shouldBe` [ Clover, Clover, Clover, Clover ]
      plants "Charlie" `shouldBe` [ Grass , Grass , Grass , Grass  ]

    it "full garden" $ do

      let plants s  = lookupPlants s $ defaultGarden plantList
          plantList = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"

      plants "Alice"   `shouldBe` [ Violets , Radishes, Violets , Radishes ]
      plants "Bob"     `shouldBe` [ Clover  , Grass   , Clover  , Clover   ]
      plants "Kincaid" `shouldBe` [ Grass   , Clover  , Clover  , Grass    ]
      plants "Larry"   `shouldBe` [ Grass   , Violets , Clover  , Violets  ]

    it  "non-alphabetical student list" $ do

      let plants s  = lookupPlants s $ garden students plantList
          plantList = "VCRRGVRG\nRVGCCGCV"
          students  = ["Samantha", "Patricia", "Xander", "Roger"]

      plants "Patricia" `shouldBe` [ Violets , Clover  , Radishes, Violets ]
      plants "Roger"    `shouldBe` [ Radishes, Radishes, Grass   , Clover  ]
      plants "Samantha" `shouldBe` [ Grass   , Violets , Clover  , Grass   ]
      plants "Xander"   `shouldBe` [ Radishes, Grass   , Clover  , Violets ]
