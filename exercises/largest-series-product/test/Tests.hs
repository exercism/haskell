{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Series (largestProduct)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "largest-series-product" $

    describe "largestProduct" $ do

      it "can find the largest product of 2 with numbers in order" $
        largestProduct 2 "0123456789"
        `shouldBe` Just 72

      it "can find the largest product of 2" $
        largestProduct 2 "576802143"
        `shouldBe` Just 48

      it "finds the largest product if span equals length" $
        largestProduct 2 "29"
        `shouldBe` Just 18

      it "can find the largest product of 3 with numbers in order" $
        largestProduct 3 "0123456789"
        `shouldBe` Just 504

      it "can find the largest product of 3" $
        largestProduct 3 "1027839564"
        `shouldBe` Just 270

      it "can find the largest product of 5 with numbers in order" $
        largestProduct 5 "0123456789"
        `shouldBe` Just 15120

      it "can get the largest product of a big number" $
        largestProduct 6 "73167176531330624919225119674426574742355349194934"
        `shouldBe` Just 23520

      it "can get the largest product of a big number II" $
        largestProduct 6 "52677741234314237566414902593461595376319419139427"
        `shouldBe` Just 28350

      it "can get the largest product of a big number (Project Euler)" $
        largestProduct 13 "73167176531330624919225119674426574742355349194934\
                          \96983520312774506326239578318016984801869478851843\
                          \85861560789112949495459501737958331952853208805511\
                          \12540698747158523863050715693290963295227443043557\
                          \66896648950445244523161731856403098711121722383113\
                          \62229893423380308135336276614282806444486645238749\
                          \30358907296290491560440772390713810515859307960866\
                          \70172427121883998797908792274921901699720888093776\
                          \65727333001053367881220235421809751254540594752243\
                          \52584907711670556013604839586446706324415722155397\
                          \53697817977846174064955149290862569321978468622482\
                          \83972241375657056057490261407972968652414535100474\
                          \82166370484403199890008895243450658541227588666881\
                          \16427171479924442928230863465674813919123162824586\
                          \17866458359124566529476545682848912883142607690042\
                          \24219022671055626321111109370544217506941658960408\
                          \07198403850962455444362981230987879927244284909188\
                          \84580156166097919133875499200524063689912560717606\
                          \05886116467109405077541002256983155200055935729725\
                          \71636269561882670428252483600823257530420752963450"
        `shouldBe` Just 23514624000

      it "reports zero if the only digits are zero" $
        largestProduct 2 "0000"
        `shouldBe` Just 0

      it "reports zero if all spans include zero" $
        largestProduct 3 "99099"
        `shouldBe` Just 0

      it "rejects span longer than string length" $
        largestProduct 4 "123"
        `shouldBe` Nothing

      it "reports 1 for empty string and empty product (0 span)" $
        largestProduct 0 ""
        `shouldBe` Just 1

      it "reports 1 for nonempty string and empty product (0 span)" $
        largestProduct 0 "123"
        `shouldBe` Just 1

      it "rejects empty string and nonzero span" $
        largestProduct 1 ""
        `shouldBe` Nothing

      it "rejects invalid character in digits" $
        largestProduct 2 "1234a5"
        `shouldBe` Nothing

      it "rejects negative span" $
        largestProduct (-1) "12345"
        `shouldBe` Nothing
