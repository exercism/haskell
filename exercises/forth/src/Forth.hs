{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
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

formatStack :: ForthState -> Text
formatStack = undefined
