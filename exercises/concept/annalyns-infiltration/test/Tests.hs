import AnnalynsInfiltration
  ( canFastAttack,
    canFreePrisoner,
    canSignalPrisoner,
    canSpy,
  )
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "AnnalynsInfiltration" $ do
    it "Cannot execute fast attack if knight is awake" $
      do
        let knightIsAwake = True
         in canFastAttack knightIsAwake
              `shouldBe` False

    it "Can execute fast attack if knight is sleeping" $
      do
        let knightIsAwake = False
         in canFastAttack knightIsAwake
              `shouldBe` True

    it "Cannot spy if everyone is sleeping" $
      do
        let knightIsAwake = False
            archerIsAwake = False
            prisonerIsAwake = False
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` False

    it "Can spy if everyone but knight is sleeping" $
      do
        let knightIsAwake = True
            archerIsAwake = False
            prisonerIsAwake = False
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can spy if everyone but archer is sleeping" $
      do
        let knightIsAwake = False
            archerIsAwake = True
            prisonerIsAwake = False
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can spy if everyone but prisoner is sleeping" $
      do
        let knightIsAwake = False
            archerIsAwake = False
            prisonerIsAwake = True
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can spy if only knight is sleeping" $
      do
        let knightIsAwake = False
            archerIsAwake = True
            prisonerIsAwake = True
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can spy if only archer is sleeping" $
      do
        let knightIsAwake = True
            archerIsAwake = False
            prisonerIsAwake = True
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can spy if only prisoner is sleeping" $
      do
        let knightIsAwake = True
            archerIsAwake = True
            prisonerIsAwake = False
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can spy if everyone is awake" $
      do
        let knightIsAwake = True
            archerIsAwake = True
            prisonerIsAwake = True
         in canSpy knightIsAwake archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Can signal prisoner if archer is sleeping and prisoner is awake" $
      do
        let archerIsAwake = False
            prisonerIsAwake = True
         in canSignalPrisoner archerIsAwake prisonerIsAwake
              `shouldBe` True

    it "Cannot signal prisoner if archer is awake and prisoner is sleeping" $
      do
        let archerIsAwake = True
            prisonerIsAwake = False
         in canSignalPrisoner archerIsAwake prisonerIsAwake
              `shouldBe` False

    it "Cannot signal prisoner if archer and prisoner are both sleeping" $
      do
        let archerIsAwake = False
            prisonerIsAwake = False
         in canSignalPrisoner archerIsAwake prisonerIsAwake
              `shouldBe` False

    it "Cannot signal prisoner if archer and prisoner are both awake" $
      do
        let archerIsAwake = True
            prisonerIsAwake = True
         in canSignalPrisoner archerIsAwake prisonerIsAwake
              `shouldBe` False

    it "Cannot release prisoner if everyone is awake and pet dog is present" $
      do
        let knightIsAwake = True
            archerIsAwake = True
            prisonerIsAwake = True
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Cannot release prisoner if everyone is awake and pet dog is absent" $
      do
        let knightIsAwake = True
            archerIsAwake = True
            prisonerIsAwake = True
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Can release prisoner if everyone is asleep and pet dog is present" $
      do
        let knightIsAwake = False
            archerIsAwake = False
            prisonerIsAwake = False
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` True

    it "Cannot release prisoner if everyone is asleep and pet dog is absent" $
      do
        let knightIsAwake = False
            archerIsAwake = False
            prisonerIsAwake = False
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Can release prisoner if only prisoner is awake and pet dog is present" $
      do
        let knightIsAwake = False
            archerIsAwake = False
            prisonerIsAwake = True
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` True

    it "Can release prisoner if only prisoner is awake and pet dog is absent" $
      do
        let knightIsAwake = False
            archerIsAwake = False
            prisonerIsAwake = True
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` True

    it "Cannot release prisoner if only archer is awake and pet dog is present" $
      do
        let knightIsAwake = False
            archerIsAwake = True
            prisonerIsAwake = False
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Cannot release prisoner if only archer is awake and pet dog is absent" $
      do
        let knightIsAwake = False
            archerIsAwake = True
            prisonerIsAwake = False
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Can release prisoner if only knight is awake and pet dog is present" $
      do
        let knightIsAwake = True
            archerIsAwake = False
            prisonerIsAwake = False
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` True
    it "Cannot release prisoner if only knight is awake and pet dog is absent" $
      do
        let knightIsAwake = True
            archerIsAwake = False
            prisonerIsAwake = False
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Cannot release prisoner if only knight is asleep and pet dog is present" $
      do
        let knightIsAwake = False
            archerIsAwake = True
            prisonerIsAwake = True
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Cannot release prisoner if only knight is asleep and pet dog is absent" $
      do
        let knightIsAwake = False
            archerIsAwake = True
            prisonerIsAwake = True
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Can release prisoner if only archer is asleep and pet dog is present" $
      do
        let knightIsAwake = True
            archerIsAwake = False
            prisonerIsAwake = True
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` True

    it "Cannot release prisoner if only archer is asleep and pet dog is absent" $
      do
        let knightIsAwake = True
            archerIsAwake = False
            prisonerIsAwake = True
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Cannot release prisoner if only prisoner is asleep and pet dog is present" $
      do
        let knightIsAwake = True
            archerIsAwake = True
            prisonerIsAwake = False
            petDogIsPresent = True
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False

    it "Cannot release prisoner if only prisoner is asleep and pet dog is absent" $
      do
        let knightIsAwake = True
            archerIsAwake = True
            prisonerIsAwake = False
            petDogIsPresent = False
         in canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent
              `shouldBe` False
