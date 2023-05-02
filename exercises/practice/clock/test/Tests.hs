{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Clock (addDelta, fromHourMin, toString)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
  describe "create" $ for_ createCases createTest
  describe "add"    $ for_ addCases    addTest
  describe "sub"    $ for_ subCases    subTest
  describe "equal"  $ for_ equalCases  equalTest

  where

    createTest (l, h, m, e) = it l assertion
      where
        assertion = toString (fromHourMin h m) `shouldBe` e

    addTest (l, h, m, m', e) = it l assertion
      where
        assertion = toString (addDelta (m' `div` 60) (m' `mod` 60) $ fromHourMin h m) `shouldBe` e

    subTest (l, h, m, m', e) = it l assertion
      where
        assertion = toString (addDelta (-m' `div` 60) (-m' `mod` 60) $ fromHourMin h m) `shouldBe` e

    equalTest (l, (h1, m1), (h2, m2), e) = it l assertion
      where
        assertion = fromHourMin h1 m1 == fromHourMin h2 m2 `shouldBe` e

    createCases =
      [ ("on the hour"                                          ,    8,     0, "08:00")
      , ("past the hour"                                        ,   11,     9, "11:09")
      , ("midnight is zero hours"                               ,   24,     0, "00:00")
      , ("hour rolls over"                                      ,   25,     0, "01:00")
      , ("hour rolls over continuously"                         ,  100,     0, "04:00")
      , ("sixty minutes is next hour"                           ,    1,    60, "02:00")
      , ("minutes roll over"                                    ,    0,   160, "02:40")
      , ("minutes roll over continuously"                       ,    0,  1723, "04:43")
      , ("hour and minutes roll over"                           ,   25,   160, "03:40")
      , ("hour and minutes roll over continuously"              ,  201,  3001, "11:01")
      , ("hour and minutes roll over to exactly midnight"       ,   72,  8640, "00:00")
      , ("negative hour"                                        ,   -1,    15, "23:15")
      , ("negative hour rolls over"                             ,  -25,     0, "23:00")
      , ("negative hour rolls over continuously"                ,  -91,     0, "05:00")
      , ("negative minutes"                                     ,    1,   -40, "00:20")
      , ("negative minutes roll over"                           ,    1,  -160, "22:20")
      , ("negative minutes roll over continuously"              ,    1, -4820, "16:40")
      , ("negative sixty minutes is previous hour"              ,    2,   -60, "01:00")
      , ("negative hour and minutes both roll over"             ,  -25,  -160, "20:20")
      , ("negative hour and minutes both roll over continuously", -121, -5810, "22:10") ]

    addCases =
      [ ("add minutes"                                   , 10,  0,     3, "10:03")
      , ("add no minutes"                                ,  6, 41,     0, "06:41")
      , ("add to next hour"                              ,  0, 45,    40, "01:25")
      , ("add more than one hour"                        , 10,  0,    61, "11:01")
      , ("add more than two hours with carry"            ,  0, 45,   160, "03:25")
      , ("add across midnight"                           , 23, 59,     2, "00:01")
      , ("add more than one day (1500 min = 25 hrs)"     ,  5, 32,  1500, "06:32")
      , ("add more than two days"                        ,  1,  1,  3500, "11:21") ]

    subCases =
      [ ("subtract minutes"                              , 10,  3,     3, "10:00")
      , ("subtract to previous hour"                     , 10,  3,    30, "09:33")
      , ("subtract more than an hour"                    , 10,  3,    70, "08:53")
      , ("subtract across midnight"                      ,  0,  3,     4, "23:59")
      , ("subtract more than two hours"                  ,  0,  0,   160, "21:20")
      , ("subtract more than two hours with borrow"      ,  6, 15,   160, "03:35")
      , ("subtract more than one day (1500 min = 25 hrs)",  5, 32,  1500, "04:32")
      , ("subtract more than two days"                   ,  2, 20,  3000, "00:20") ]

    equalCases =
      [ ("clocks with same time"                                , (15, 37), ( 15,     37), True )
      , ("clocks a minute apart"                                , (15, 36), ( 15,     37), False)
      , ("clocks an hour apart"                                 , (14, 37), ( 15,     37), False)
      , ("clocks with hour overflow"                            , (10, 37), ( 34,     37), True )
      , ("clocks with hour overflow by several days"            , ( 3, 11), ( 99,     11), True )
      , ("clocks with negative hour"                            , (22, 40), ( -2,     40), True )
      , ("clocks with negative hour that wraps"                 , (17,  3), (-31,      3), True )
      , ("clocks with negative hour that wraps multiple times"  , (13, 49), (-83,     49), True )
      , ("clocks with minute overflow"                          , ( 0,  1), (  0,   1441), True )
      , ("clocks with minute overflow by several days"          , ( 2,  2), (  2,   4322), True )
      , ("clocks with negative minute"                          , ( 2, 40), (  3,    -20), True )
      , ("clocks with negative minute that wraps"               , ( 4, 10), (  5,  -1490), True )
      , ("clocks with negative minute that wraps multiple times", ( 6, 15), (  6,  -4305), True )
      , ("clocks with negative hours and minutes"               , ( 7, 32), (-12,   -268), True )
      , ("clocks with negative hours and minutes that wrap"     , (18,  7), (-54, -11513), True )
      , ("full clock and zeroed clock"                          , (24,  0), (  0,      0), True )
      ]
