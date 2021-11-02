module SecretHandshake (handshake) where

import Data.Bits (testBit)

data Action = Wink
            | DoubleBlink
            | CloseYourEyes
            | Jump
            | Reverse
            deriving (Show, Eq, Enum, Bounded)

handshake :: Int -> [String]
handshake = foldr f [] . toActions
  where f Reverse acc = reverse acc
        f action acc = toString action:acc
        toString action = case action of
          Wink          -> "wink"
          DoubleBlink   -> "double blink"
          CloseYourEyes -> "close your eyes"
          Jump          -> "jump"
          Reverse       -> undefined

toActions :: Int -> [Action]
toActions n = [act | act <- Reverse:[Wink .. Jump], n `testBit` fromEnum act]
