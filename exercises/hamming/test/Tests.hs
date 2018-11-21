{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List         ((\\))
import Data.Ord          (comparing)
import Test.QuickCheck

import Hamming (distance)

instance Arbitrary Case where
  arbitrary = sized hammingGen
  shrink = hammingShrink

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "distance" $ do
  for_ cases test
  it "works in general" (property test')
  where
    test Case{..} = it description (distance strand1 strand2 `shouldBe` (fromIntegral <$> expected))
    test' Case{..} = distance strand1 strand2 === (fromIntegral <$> expected)

data Case = Case { description :: String
                 , strand1     :: String
                 , strand2     :: String
                 , expected    :: Maybe Integer
                 }
            deriving (Show, Eq)

cases :: [Case]
cases = [ Case { description = "empty strands"
               , strand1     = ""
               , strand2     = ""
               , expected    = Just 0
               }
        , Case { description = "identical strands"
               , strand1     = "A"
               , strand2     = "A"
               , expected    = Just 0
               }
        , Case { description = "long identical strands"
               , strand1     = "GGACTGA"
               , strand2     = "GGACTGA"
               , expected    = Just 0
               }
        , Case { description = "complete distance in single nucleotide strands"
               , strand1     = "A"
               , strand2     = "G"
               , expected    = Just 1
               }
        , Case { description = "complete distance in small strands"
               , strand1     = "AG"
               , strand2     = "CT"
               , expected    = Just 2
               }
        , Case { description = "small distance in small strands"
               , strand1     = "AT"
               , strand2     = "CT"
               , expected    = Just 1
               }
        , Case { description = "small distance"
               , strand1     = "GGACG"
               , strand2     = "GGTCG"
               , expected    = Just 1
               }
        , Case { description = "small distance in long strands"
               , strand1     = "ACCAGGG"
               , strand2     = "ACTATGG"
               , expected    = Just 2
               }
        , Case { description = "non-unique character in first strand"
               , strand1     = "AAG"
               , strand2     = "AAA"
               , expected    = Just 1
               }
        , Case { description = "non-unique character in second strand"
               , strand1     = "AAA"
               , strand2     = "AAG"
               , expected    = Just 1
               }
        , Case { description = "same nucleotides in different positions"
               , strand1     = "TAG"
               , strand2     = "GAT"
               , expected    = Just 2
               }
        , Case { description = "large distance"
               , strand1     = "GATACA"
               , strand2     = "GCATAA"
               , expected    = Just 4
               }
        , Case { description = "large distance in off-by-one strand"
               , strand1     = "GGACGGATTCTG"
               , strand2     = "AGGACGGATTCT"
               , expected    = Just 9
               }
        , Case { description = "disallow first strand longer"
               , strand1     = "AATG"
               , strand2     = "AAA"
               , expected    = Nothing
               }
        , Case { description = "disallow second strand longer"
               , strand1     = "ATA"
               , strand2     = "AGTG"
               , expected    = Nothing
               }
        ]

hammingGen :: Int -> Gen Case
hammingGen 0 = return
  Case { description = "Empty strands have no distance"
       , strand1 = ""
       , strand2 = ""
       , expected = Just 0
       }

hammingGen n = do
  (a, b, count) <- nucleotidePairGen
  case' <- hammingGen (n-1)
  return . hammingDesc $
    case' { description = ""
          , strand1 = a ++ strand1 case'
          , strand2 = b ++ strand2 case'
          , expected = (+) <$> count <*> expected case'
          }

hammingDesc :: Case -> Case
hammingDesc case'@Case{..} = case' { description = desc' }
  where
    desc' = "Strands of " ++ length' ++ " with " ++ dist' ++ " distance"
    dist' = maybe "invalid" show expected
    length' = case comparing length strand1 strand2 of
      EQ -> "lengths " ++ show (length strand1)
      _  -> "different lengths " ++ show (length strand1) ++ " and " ++
            show (length strand2)

nucleotidePairGen :: Gen (String, String, Maybe Integer)
nucleotidePairGen = frequency
  [ (20, zeroDistance)
  , (4, oneDistance)
  , (1, invalidDistance)
  ]
  where
    zeroDistance = do
      a <- elements "ACGT"
      return ([a], [a], Just 0)

    oneDistance = do
      a <- elements "ACGT"
      b <- elements ("ACGT" \\ [a])
      return ([a], [b], Just 1)

    invalidDistance = do
      a <- elements "ACGT"
      return ([a], [], Nothing)

-- | `select [1,2,3]` = `[(1,[2,3]),(2,[1,3]),(3,[1,2])]`.
select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : map (fmap (x:)) (select xs)

hammingShrink :: Case -> [Case]
hammingShrink case'@Case{..} = map hammingDesc $
  zipWith shrink' (select strand1) (select strand2)
  where
    shrink' (a, strand1') (b, strand2') =
      case' { strand1 = strand1'
            , strand2 = strand2'
            , expected = (if a == b then id else pred) <$> expected }
