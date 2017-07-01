module Prelude
  ( module Exports
  , hspecFastFail
  ) where

-- Re-exports

import BasePrelude as Exports
import Test.Hspec  as Exports
  ( Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldMatchList
  , shouldNotBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )

-- Local imports

import Test.Hspec.Runner
  ( configFastFail
  , defaultConfig
  , hspecWith
  )

-- Additional exports

hspecFastFail :: Spec -> IO ()
hspecFastFail = hspecWith defaultConfig {configFastFail = True}
