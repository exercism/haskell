import Data.Time.Clock   (UTCTime)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Gigasecond (fromDay)

import Data.Time.Format
  ( ParseTime
  , TimeLocale
  , defaultTimeLocale
  , iso8601DateFormat
  , parseTimeOrError
  )

readTime :: ParseTime t => TimeLocale -> String -> String -> t
readTime = parseTimeOrError True

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "fromDay" $ do

          let dt = readTime defaultTimeLocale
                   (iso8601DateFormat (Just "%T%Z")) :: String -> UTCTime

          it "from apr 25 2011" $
            fromDay (dt "2011-04-25T00:00:00Z")
            `shouldBe` dt "2043-01-01T01:46:40Z"

          it "from jun 13 1977" $
            fromDay (dt "1977-06-13T00:00:00Z")
            `shouldBe` dt "2009-02-19T01:46:40Z"

          it "from jul 19 1959" $
            fromDay (dt "1959-07-19T00:00:00Z")
            `shouldBe` dt "1991-03-27T01:46:40Z"
