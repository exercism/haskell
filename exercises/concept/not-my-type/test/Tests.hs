import           Control.Monad (forM_)
import qualified NotMyType     (answer)
import           Test.Hspec    (describe, hspec, it, shouldBe)
import           Text.Printf   (printf)

questions :: [[Char]]
questions = [ "'1'"
            , "1.0"
            , "A string"
            , "[1,2,3]"
            , "(1,'a')"
            , "(+)"
            ]

answers :: [[Char]]
answers = map snd NotMyType.answer

expect :: [[Char]]
expect = ["Char"
         , "Double"
         , "[Char]"
         , "[Integer]"
         , "(Integer, Char)"
         , "Integer -> Integer -> Integer"
         ]

input :: [([Char], [Char], [Char])]
input = zip3 questions answers expect

main :: IO ()
main = hspec $ do
       describe "You said:" $ do
          forM_ input $ \(question, answer, expected) ->
            it (printf "%s has type %s" question answer) $
            answer `shouldBe` expected
