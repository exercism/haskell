{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import Data.Text (Text)

data ForthState -- TODO: define this data type

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

empty :: ForthState
empty = error "TODO: An empty ForthState"

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText = error "TODO: Evaluate an input Text, returning the new state"

formatStack :: ForthState -> Text
formatStack = error "TODO: Return the current stack as Text with the element \
                    \on top of the stack being the rightmost element in the \
                    \output"

