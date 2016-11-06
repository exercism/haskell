{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , empty
  ) where

import Data.Text (Text)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

empty :: ForthState
empty = undefined

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = undefined

toList :: ForthState -> [Int]
toList = undefined
