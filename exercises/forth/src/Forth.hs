{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = Dummy

emptyState :: ForthState
emptyState = error "You need to implement this function."

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text stack = error "You need to implement this function."

toList :: ForthState -> [Int]
toList stack = error "You need to implement this function."
