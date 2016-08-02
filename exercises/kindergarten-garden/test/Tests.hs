import Test.Hspec        (Spec, describe, it, shouldBe)
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
specs = describe "kindergarten-garden" $ do

    -- As of 2016-08-02, there was no reference file
    -- for the test cases in `exercism/x-common`.

    it "alice tests" $ do

      let alicePlants = lookupPlants "Alice" . defaultGarden

      alicePlants "RC\nGG" `shouldBe` [ Radishes, Clover, Grass   , Grass  ]
      alicePlants "VC\nRC" `shouldBe` [ Violets , Clover, Radishes, Clover ]

    it "small garden" $ do

      let plants s  = lookupPlants s $ defaultGarden plantList
          plantList = "VVCG\nVVRC"

      plants "Bob" `shouldBe` [ Clover, Grass, Radishes, Clover ]

    it "medium garden" $ do

      let plants s  = lookupPlants s $ defaultGarden plantList
          plantList = "VVCCGG\nVVCCGG"

      plants "Bob"     `shouldBe` [ Clover, Clover, Clover, Clover ]
      plants "Charlie" `shouldBe` [ Grass , Grass , Grass , Grass  ]

    it "full garden" $ do

      let plants s  = lookupPlants s $ defaultGarden plantList
          plantList = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"

      plants "Alice"   `shouldBe` [ Violets , Radishes, Violets , Radishes ]
      plants "Bob"     `shouldBe` [ Clover  , Grass   , Clover  , Clover   ]
      plants "Charlie" `shouldBe` [ Violets , Violets , Clover  , Grass    ]
      plants "David"   `shouldBe` [ Radishes, Violets , Clover  , Radishes ]
      plants "Eve"     `shouldBe` [ Clover  , Grass   , Radishes, Grass    ]
      plants "Fred"    `shouldBe` [ Grass   , Clover  , Violets , Clover   ]
      plants "Ginny"   `shouldBe` [ Clover  , Grass   , Grass   , Clover   ]
      plants "Harriet" `shouldBe` [ Violets , Radishes, Radishes, Violets  ]
      plants "Ileana"  `shouldBe` [ Grass   , Clover  , Violets , Clover   ]
      plants "Joseph"  `shouldBe` [ Violets , Clover  , Violets , Grass    ]
      plants "Kincaid" `shouldBe` [ Grass   , Clover  , Clover  , Grass    ]
      plants "Larry"   `shouldBe` [ Grass   , Violets , Clover  , Violets  ]

    it  "surprise garden" $ do

      let plants s  = lookupPlants s $ garden students plantList
          plantList = "VCRRGVRG\nRVGCCGCV"
          students  = ["Samantha", "Patricia", "Xander", "Roger"]

      plants "Patricia" `shouldBe` [ Violets , Clover  , Radishes, Violets ]
      plants "Roger"    `shouldBe` [ Radishes, Radishes, Grass   , Clover  ]
      plants "Samantha" `shouldBe` [ Grass   , Violets , Clover  , Grass   ]
      plants "Xander"   `shouldBe` [ Radishes, Grass   , Clover  , Violets ]
