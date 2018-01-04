{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , empty
  ) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Char (isSpace, isControl)
import qualified Data.Text as T
import qualified Data.Text.Read as R
import qualified Data.Map.Strict as M
import Control.Arrow (first)

type Value = Int
type FWord = Text
data Term
     = StartDefinition
     | EndDefinition
     | V Value
     | W FWord
     deriving (Show, Ord, Eq)

data Definition
     = Add
     | Subtract
     | Multiply
     | Divide
     | Dup
     | Drop
     | Swap
     | Over
     | User [Term]
     deriving (Show, Eq)

data ForthState = ForthState
     { forthStack :: [Value]
     , forthCode  :: [Term]
     , forthWords :: Map FWord Definition
     }
     deriving (Show, Eq)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord FWord
     deriving (Show, Eq)

defaultWords :: Map FWord Definition
defaultWords = M.fromList . map (first T.toCaseFold) $
  [ ("+", Add)
  , ("-", Subtract)
  , ("*", Multiply)
  , ("/", Divide)
  , ("dup", Dup)
  , ("drop", Drop)
  , ("swap", Swap)
  , ("over", Over)
  ]

empty :: ForthState
empty = ForthState
      { forthStack = []
      , forthCode  = []
      , forthWords = defaultWords
      }

parseText :: Text -> [Term]
parseText =
  map toTerm . filter (not . T.null) . T.split (\c -> isSpace c || isControl c)
  where
    toTerm ":" = StartDefinition
    toTerm ";" = EndDefinition
    toTerm w = case R.signed R.decimal w of
        Right (n, "") -> V n
        _             -> W (T.toCaseFold w)

eval :: [Term] -> ForthState -> Either ForthError ForthState
eval code state = mapCode (++code) state >>= runInterpreter

runInterpreter :: ForthState -> Either ForthError ForthState
runInterpreter s = case forthCode s of
  []     -> return s
  (x:xs) -> step x s { forthCode = xs }

with2 :: (Value -> Value -> ForthState -> Either ForthError ForthState)
      -> ForthState
      -> Either ForthError ForthState
with2 f s = case forthStack s of
  (a:b:xs) -> f b a s { forthStack = xs }
  _        -> Left StackUnderflow

with1 :: (Value -> ForthState -> Either ForthError ForthState)
      -> ForthState
      -> Either ForthError ForthState
with1 f s = case forthStack s of
  (x:xs) -> f x s { forthStack = xs }
  _      -> Left StackUnderflow

step :: Term -> ForthState -> Either ForthError ForthState
step t s = case t of
  StartDefinition -> case break (EndDefinition ==) (forthCode s) of
    (W w:xs, EndDefinition:ys) ->
      runInterpreter s { forthWords = M.insert w (User xs) (forthWords s)
                       , forthCode  = ys
                       }
    (_, EndDefinition:_) -> Left InvalidWord
    _                    -> return s { forthCode = t : forthCode s  }
  EndDefinition   -> Left InvalidWord
  V v             -> push v s >>= runInterpreter
  W w             ->
    maybe (Left (UnknownWord w)) (`stepWord` s) (M.lookup w (forthWords s)) >>=
    runInterpreter

push :: Value -> ForthState -> Either ForthError ForthState
push = mapStack . ((:) $!)

mapStack :: ([Value] -> [Value]) -> ForthState -> Either ForthError ForthState
mapStack f s = return s { forthStack = f (forthStack s) }

mapCode :: ([Term] -> [Term]) -> ForthState -> Either ForthError ForthState
mapCode f s = return s { forthCode = f (forthCode s) }

safeDiv :: Value -> Value -> Either ForthError Value
safeDiv a b
  | b /= 0    = return (a `div` b)
  | otherwise = Left DivisionByZero

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

stepWord :: Definition -> ForthState -> Either ForthError ForthState
stepWord Add       = with2 (push .: (+))
stepWord Subtract  = with2 (push .: (-))
stepWord Multiply  = with2 (push .: (*))
stepWord Divide    = with2 (either (const . Left) push .: safeDiv)
stepWord Dup       = with1 (mapStack . (++) . replicate 2)
stepWord Drop      = with1 (const return)
stepWord Swap      = with2 (\a b -> mapStack ([a, b] ++))
stepWord Over      = with2 (\a b -> mapStack ([a, b, a] ++))
stepWord (User xs) = mapCode (xs ++)

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = eval . parseText

toList :: ForthState -> [Int]
toList = reverse . forthStack
