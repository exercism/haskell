module AnnalynsInfiltration (canFastAttack, canSpy, canSignalPrisoner, canFreePrisoner) where

canFastAttack :: Bool -> Bool
canFastAttack knightIsAwake = error "Implement the 'canFastAttack' function"

canSpy :: Bool-> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    error "Implement the 'canSpy' function"

canSignalPrisoner :: Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    error "Implement the 'canSignalPrisoner' function"


canFreePrisoner :: Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    error "Implement the 'canFreePrisoner' function"
