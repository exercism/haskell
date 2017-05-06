{-# LANGUAGE OverloadedStrings #-}

-- Basic imports
import Control.Applicative ((<|>), liftA2)
import Control.Monad       ((>=>))

-- To construct the tests.
import Test.Hspec          (Spec, describe, it)
import Test.Hspec.Runner   (configFastFail, defaultConfig, hspecWith)
import Test.HUnit          (assertEqual)

-- To parse the JSON file.
import Data.Aeson          ((.:), eitherDecodeStrict', withArray, withObject)
import Data.Aeson.Types    (Parser, Value, parseEither)
import GHC.Exts            (toList)

-- To read the JSON file.
import Data.ByteString     (readFile)
import Prelude     hiding  (readFile)

-- The module to be tested.
import Bob (responseFor)

-- Read, decode and run the tests.
main :: IO ()
main = readJSON >>= parseOrError parseJSON >>= runTests
  where
    readJSON     = readFile "test/canonical-data.json"
    parseOrError = (either error pure .)
    parseJSON    = eitherDecodeStrict' >=> parseEither parseCanonical
    runTests     = hspecWith defaultConfig {configFastFail = True}

-- | Top-level JSON parser.
--
-- Depends on 'parsers' to parse single test cases.
parseCanonical :: Value -> Parser Spec
parseCanonical = parseTop
  where
    parseTop = withObject "top-level" $ \o -> do
      exercise <- o .: "exercise"
      version  <- o .: "version"
      specs    <- o .: "group" >>= parseGroup
      let topName = exercise ++ "-" ++ version
      return . describe topName . sequence_ $ specs

    parseGroup = withArray "group" (traverse parseItem . toList)

    parseItem i = parseLabeledGroup i <|> parseTest i

    parseLabeledGroup = withObject "group" $ \o -> do
      description <- o .: "description"
      specs       <- o .: "group" >>= parseGroup
      return . describe description . sequence_ $ specs

    parseTest = foldr1 (liftA2 (<|>)) parsers

--
-- Exercise-specific code.
--

-- List of exercise-specific test case parsers.
parsers :: [Value -> Parser Spec]
parsers = [parseResponse]

-- | Parse a "response"-type test case.
parseResponse :: Value -> Parser Spec
parseResponse = withObject "response" $ \t -> do
    o           <- t .: "response"
    description <- o .: "description"
    input       <- o .: "input"
    expected    <- o .: "expected"
    return $ it description $
                  assertEqual ("responseFor " ++ show input)
                    expected
                    (responseFor input)
