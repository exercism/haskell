import Data.Bifunctor    (first)
import Data.List         (foldl')
import Data.Set          (toAscList)
import Data.Tuple        (swap)
import Test.Hspec        (Spec, it, shouldBe, shouldMatchList)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import qualified Data.Map as Map

import Counting (Color(Black,White), territories, territoryFor)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    let board5x5 = [ "  B  "
                   , " B B "
                   , "B W B"
                   , " W W "
                   , "  W  " ]

        shouldHaveTerritories = shouldMatchList
                              . map (first toAscList)
                              . territories

        add m (owner, size) = Map.insertWith (+) owner size m

        shouldScore = shouldMatchList
                    . Map.toList
                    . foldl' add Map.empty
                    . map (swap . first length)
                    . territories

        territoryIn xss = fmap (first toAscList) . territoryFor xss

    it "minimal board, no territories" $
      [ "B" ] `shouldHaveTerritories` []

    it "one territory, covering the whole board" $
      [ " " ] `shouldHaveTerritories` [([ (1, 1) ], Nothing)]

    it "two territories, rectangular board" $
      [ " BW "
      , " BW " ] `shouldHaveTerritories` [ ([ (1, 1)
                                            , (1, 2) ], Just Black)
                                         , ([ (4, 1)
                                            , (4, 2) ], Just White) ]

    it "two territories of same player, rectangular board" $
      [ " B " ] `shouldHaveTerritories` [ ([ (1, 1) ], Just Black)
                                        , ([ (3, 1) ], Just Black) ]

    it "5x5 score" $
      board5x5 `shouldScore` [ (Nothing   , 9)
                             , (Just Black, 6)
                             , (Just White, 1) ]

    it "5x5 territory for black bordering edge" $
      territoryIn board5x5 (1, 2) `shouldBe` Just ([ (1, 1)
                                                   , (1, 2)
                                                   , (2, 1) ], Just Black)

    it "5x5 territory for white not bordering edge" $
      territoryIn board5x5 (3, 4) `shouldBe` Just ([ (3, 4) ], Just White)

    it "5x5 open territory bordering edge" $
      territoryIn board5x5 (2, 5) `shouldBe` Just ([ (1, 4)
                                                   , (1, 5)
                                                   , (2, 5) ], Nothing)

    it "5x5 non-territory (stone)" $
      territoryIn board5x5 (2, 2) `shouldBe` Nothing

    it "5x5 non-territory (X too low)" $
      territoryIn board5x5 (0, 2) `shouldBe` Nothing

    it "5x5 non-territory (X too high)" $
      territoryIn board5x5 (6, 2) `shouldBe` Nothing

    it "5x5 non-territory (Y too low)" $
      territoryIn board5x5 (2, 0) `shouldBe` Nothing

    it "5x5 non-territory (Y too high)" $
      territoryIn board5x5 (2, 6) `shouldBe` Nothing
