{-# LANGUAGE GADTs #-}
module Alphametics (solve) where

import Control.Monad (guard)
import Data.List (foldl', find, union)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Prelude hiding (Word)
import Text.Parsec
import Text.Parsec.String

type Solution = [(Char, Int)]

solve :: String -> Maybe Solution
solve puzzle = do
  boolTerm' <- parseTerm puzzle
  let columns = [0..maxLength boolTerm' - 1]
      emptySolutions = [([], (0, 0))]
      solutions = foldl' (flip $ partialSolutions boolTerm') emptySolutions columns
  fst <$> listToMaybe solutions

parseTerm :: String -> Maybe (Term Bool)
parseTerm str = either (const Nothing) Just parseResult
  where parseResult = parse boolTerm "boolTerm" str

type PartialResult = (Integer, Integer) -- (result, carry)
type PartialSolution = (Solution, PartialResult)

partialSolutions :: Term a -> Int -> [PartialSolution] -> [PartialSolution]
partialSolutions (Word xs) col solutionsSoFar
  | col >= length xs = solutionsSoFar -- pass, no more constraints from this word
  | otherwise = mergePartialSolutions takeChar charSolutions solutionsSoFar
  where
    charSolutions = toCharSolution <$> [if firstChar then 1 else 0..9]
    char' = reverse xs !! col  -- process from the right
    firstChar = head xs == char'
    toCharSolution x = ([(char', x)], (toInteger x, 0))
    takeChar (x, _) (_, y) = Just (x, y)
partialSolutions (Plus left right) col solutionsSoFar =
  mergePartialSolutions addResults leftSolutions rightSolutions
  where
    leftSolutions = partialSolutions left col solutionsSoFar
    rightSolutions = partialSolutions right col solutionsSoFar
    addResults (x1, x2) (y1, _) = Just (x1 + y1, x2)
partialSolutions (Equals left right) col solutionsSoFar =
  mergePartialSolutions equateResults leftSolutions rightSolutions
  where
    equateResults (leftResult, leftCarry) (rightResult, _)
      | rightResult == requiredResult = Just (0, carry)
      | otherwise = Nothing
      where (carry, requiredResult) = (leftResult + leftCarry) `divMod` 10
    leftSolutions = partialSolutions left col solutionsSoFar
    rightSolutions = partialSolutions right col solutionsSoFar

mergePartialSolutions :: (PartialResult -> PartialResult -> Maybe PartialResult) ->
  [PartialSolution] -> [PartialSolution] -> [PartialSolution]
mergePartialSolutions mergeResults xs ys = do
  (xSolution, xResult) <- xs
  (ySolution, yResult) <- ys
  let mergedSolution = mergeSolutions xSolution ySolution
  guard $ (not . null) mergedSolution
  let mergedResult = mergeResults xResult yResult
  guard $ isJust mergedResult
  return (mergedSolution, fromJust mergedResult)
  where
    mergeSolutions :: Solution -> Solution -> Solution
    mergeSolutions s1 s2
      | any conflict s1 = []
      | otherwise       = s1 `union` s2
      where
        conflict (k1, v1) =
          maybe False (/= v1) (lookup k1 s2) ||
          maybe False ((/= k1) . fst) (find ((== v1) . snd) s2)

data Term a where
  Word :: String -> Term Integer
  Equals :: (Eq a) => Term a -> Term a -> Term Bool
  Plus :: Term Integer -> Term Integer -> Term Integer

maxLength :: Term a -> Int
maxLength (Word xs) = length xs
maxLength (Equals l r) = max (maxLength l) (maxLength r)
maxLength (Plus   l r) = max (maxLength l) (maxLength r)

-- Term Parser
boolTerm :: Parser (Term Bool)
boolTerm = do
  left  <- integerTerm
  _     <- trimmed (string "==")
  right <- integerTerm
  return $ Equals left right

integerTerm :: Parser (Term Integer)
integerTerm = try integerOperation <|> simpleIntegerTerm

integerOperation :: Parser (Term Integer)
integerOperation = do
  left  <- simpleIntegerTerm
  op    <- integerOperator
  right <- integerTerm
  return $ op left right

simpleIntegerTerm :: Parser (Term Integer)
simpleIntegerTerm = word

integerOperator :: Parser (Term Integer -> Term Integer -> Term Integer)
integerOperator = plus

word :: Parser (Term Integer)
word = Word <$> many1 upper

plus :: Parser (Term Integer -> Term Integer -> Term Integer)
plus = trimmed (char '+') *> pure Plus

trimmed :: Parser a -> Parser a
trimmed p = spaces *> p <* spaces

