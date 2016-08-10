import Test.Hspec        (Spec, describe, it, shouldReturn)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Deque (mkDeque, pop, push, shift, unshift)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "linked-list" $ do

    -- As of 2016-08-10, there was no reference file
    -- for the test cases in `exercism/x-common`.

    it "push pop" $ do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      pop deque `shouldReturn` Just 'b'
      pop deque `shouldReturn` Just 'a'

    it "push shift" $ do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      shift deque `shouldReturn` Just 'a'
      shift deque `shouldReturn` Just 'b'

    it "unshift shift" $ do
      deque <- mkDeque
      unshift deque 'a'
      unshift deque 'b'
      shift deque `shouldReturn` Just 'b'
      shift deque `shouldReturn` Just 'a'

    it "unshift pop" $ do
      deque <- mkDeque
      unshift deque 'a'
      unshift deque 'b'
      pop deque `shouldReturn` Just 'a'
      pop deque `shouldReturn` Just 'b'

    it "example" $ do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      pop deque `shouldReturn` Just 'b'
      push deque 'c'
      shift deque `shouldReturn` Just 'a'
      unshift deque 'd'
      push deque 'e'
      shift deque `shouldReturn` Just 'd'
      pop deque `shouldReturn` Just 'e'
      pop deque `shouldReturn` Just 'c'
