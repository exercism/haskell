module AnnalynsInfiltration (canFastAttack, canSpy, canSignalPrisoner, canFreePrisoner) where


canFastAttack :: Bool -> Bool
canFastAttack knightIsAwake =
    not knightIsAwake

canSpy :: Bool-> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    knightIsAwake || archerIsAwake || prisonerIsAwake

canSignalPrisoner :: Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    not archerIsAwake && prisonerIsAwake

canFreePrisoner :: Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    not knightIsAwake && not archerIsAwake && prisonerIsAwake || not archerIsAwake && petDogIsPresent
