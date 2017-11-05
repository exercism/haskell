{-# LANGUAGE OverloadedStrings #-}
module WordProblem (answer) where
import Data.Functor (($>))
import Data.Text (pack)
import Data.List (foldl')
import Control.Applicative ((<|>))
import Data.Attoparsec.Text
  ( Parser, signed, decimal, space, maybeResult, parse, many' )

answerParser :: Parser Int
answerParser = do
  n <- "What is " *> signed decimal
  ops <- many' (space *> operation)
  "?" $> foldl' (flip ($)) n ops

answer :: String -> Maybe Int
answer = maybeResult . parse answerParser . pack

operation :: Parser (Int -> Int)
operation = (flip <$> operator) <* space <*> signed decimal

operator :: Parser (Int -> Int -> Int)
operator = "plus"          $> (+) <|>
           "minus"         $> (-) <|>
           "multiplied by" $> (*) <|>
           "divided by"    $> div
