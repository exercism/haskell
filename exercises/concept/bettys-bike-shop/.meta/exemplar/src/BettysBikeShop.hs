{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module BettysBikeShop (penceToPounds, poundsToString) where

import qualified ToString (float)


penceToPounds :: Int -> Float
penceToPounds pence =
    toFloat pence / 100.0


poundsToString :: Float -> String
poundsToString pounds =
    "£" ++ ToString.float pounds
