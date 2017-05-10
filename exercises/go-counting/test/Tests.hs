import Data.Bifunctor    (first)
import Data.MultiSet     (fromOccurList, toOccurList)
import Data.Set          (toAscList)
import Data.Tuple        (swap)
import Test.Hspec        (Spec, it, shouldBe, shouldMatchList)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

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

        board9x9 = [ "  B   B  "
                   , "B   B   B"
                   , "WBBBWBBBW"
                   , "W W W W W"
                   , "         "
                   , " W W W W "
                   , "B B   B B"
                   , " W BBB W "
                   , "   B B   " ]

        shouldHaveTerritories = shouldMatchList
                              . map (first toAscList)
                              . territories

        shouldScore = shouldMatchList
                    . toOccurList
                    . fromOccurList
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

    it "5x5 score" $
      board5x5 `shouldScore` [ (Nothing   , 9)
                             , (Just Black, 6)
                             , (Just White, 1) ]

    it "5x5 territory for black" $
      territoryIn board5x5 (1, 2) `shouldBe` Just ([ (1, 1)
                                                   , (1, 2)
                                                   , (2, 1) ], Just Black)

    it "5x5 territory for white" $
      territoryIn board5x5 (3, 4) `shouldBe` Just ([ (3, 4) ], Just White)

    it "5x5 open territory" $
      territoryIn board5x5 (2, 5) `shouldBe` Just ([ (1, 4)
                                                   , (1, 5)
                                                   , (2, 5) ], Nothing)

    it "5x5 non-territory (stone)" $
      territoryIn board5x5 (2, 2) `shouldBe` Nothing

    it "5x5 non-territory (too low coordinate)" $
      territoryIn board5x5 (0, 2) `shouldBe` Nothing

    it "5x5 non-territory (too high coordinate)" $
      territoryIn board5x5 (2, 6) `shouldBe` Nothing

    it "9x9 score" $
      board9x9 `shouldScore` [ (Nothing   , 33)
                             , (Just Black, 14) ]
