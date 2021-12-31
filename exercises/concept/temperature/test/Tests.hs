import           Control.Monad (forM_)
import           Temperature   (tempTo)
import           Test.Hspec    (describe, hspec, it, shouldBe)
import           Text.Printf   (printf)

questions :: [(Char, Integer)]
questions  = [ ('C', -31), ('C', 32), ('C', 50), ('C', 212), ('C', 2120)
             , ('F', -35), ('F', 0),  ('F', 30), ('F', 100), ('F', 1001)
             ]

answers :: [Float]
answers = map (uncurry tempTo) questions

expect :: [Float]
expect  = [ -35.0, 0.0, 10.0, 100.0, 1160.0
          , -31.0, 32.0, 86.0, 212.0, 1833.8
          ]

input :: [((Char, Integer), Float, Float)]
input = zip3 questions answers expect

main :: IO ()
main =  hspec $ do
        describe "tempTo:" $ do
          forM_ input $ \((unitDesired, inputTemp), ans, expected) ->
            it (printf "%i%c equals %.1f%c" inputTemp (oppositeUnit unitDesired) ans unitDesired) $
            roundTo 1 ans `shouldBe` expected
  where oppositeUnit unit =
          case unit of
            'C' -> 'F'
            'F' -> 'C'
            _   -> error "Error: Unit must be 'F' or 'C'."

roundTo :: Int -> Float -> Float
roundTo n = (/ 10 ^ n) . (fromIntegral :: Integer -> Float) . round . (* 10 ^ n)
