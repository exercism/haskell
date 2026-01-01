module LuciansLusciousLasagna (totalTimeInMinutes, remainingMinutesInOven, expectedMinutesInOven, preparationTimeInMinutes, alarm) where

expectedMinutesInOven :: Int
expectedMinutesInOven =
    40

remainingMinutesInOven :: Int -> Int
remainingMinutesInOven passedAlready =
    expectedMinutesInOven - passedAlready

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes layers =
    2 * layers

totalTimeInMinutes :: Int -> Int -> Int
totalTimeInMinutes layers passedAlready =
    passedAlready + preparationTimeInMinutes layers

alarm :: String
alarm =
    "DING!"