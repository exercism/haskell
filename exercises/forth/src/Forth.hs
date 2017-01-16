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

data ForthState = Dummy

empty :: ForthState
empty = error "You need to implement this function."

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = error "You need to implement this function."

toList :: ForthState -> [Int]
toList = error "You need to implement this function."
