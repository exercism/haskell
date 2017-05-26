{-# LANGUAGE OverloadedStrings #-}

import Frequency (frequency)

import Prelude hiding (unlines)
import Data.Text (Text, unlines)

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Criterion.Types (Benchmark)
import Control.Concurrent (getNumCapabilities)
import Data.List (nub, sort, replicate)

odeAnDieFreude :: Text
odeAnDieFreude = unlines
                 [ "Freude schöner Götterfunken"
                 , "Tochter aus Elysium,"
                 , "Wir betreten feuertrunken,"
                 , "Himmlische, dein Heiligtum!"
                 , "Deine Zauber binden wieder"
                 , "Was die Mode streng geteilt;"
                 , "Alle Menschen werden Brüder,"
                 , "Wo dein sanfter Flügel weilt."
                 ]


makeBench :: [Text] -> Int -> Benchmark
makeBench anthems workers = bench name $ nf (`frequency` anthems) workers
  where name = show workers ++ " workers"

benchGroup :: Int -> [Int] -> Int -> Benchmark
benchGroup processors numWorkers numAnthems =
  bgroup (show numAnthems ++ " anthems on " ++ show processors ++ " threads") (makeBench anthems <$> numWorkers)
  where anthems = replicate numAnthems odeAnDieFreude

main :: IO ()
main = do threads <- getNumCapabilities
          let numsOfWorkers = nub $ sort [1..threads]
              numsOfAnthems = [500]

          defaultMain $ benchGroup threads numsOfWorkers <$> numsOfAnthems
