{-# LANGUAGE OverloadedStrings #-}

import Frequency (frequency)

import Prelude hiding (unlines)
import Data.Text (Text, unlines)

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Criterion.Types (Benchmark)
import Control.Concurrent (setNumCapabilities)
import GHC.Conc (getNumProcessors)
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
  bgroup (show numAnthems ++ " anthems on " ++ show processors ++ " cores") (makeBench anthems <$> numWorkers)
  where anthems = replicate numAnthems odeAnDieFreude

main :: IO ()
main = do processors <- getNumProcessors
          let numsOfWorkers = nub $ sort [1, processors]
              numsOfAnthems = [4, 500]

          -- run on 1 core
          setNumCapabilities 1
          defaultMain $ benchGroup 1 numsOfWorkers <$> numsOfAnthems

          -- run on all cores
          setNumCapabilities processors
          defaultMain $ benchGroup processors numsOfWorkers <$> numsOfAnthems
