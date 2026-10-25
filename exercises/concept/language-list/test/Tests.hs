import Test.Hspec             (it, shouldBe, hspec)
import LanguageList (new, add, remove, first, count, isFunctionalList)

main :: IO ()
main = hspec $ do
    it "new list" $ do
        new `shouldBe` []

    it "add a language to a list" $ do
        add "Haskell" new `shouldBe` ["Haskell"]
    
    it "add several languages to a list" $ do
        let updatedList = add "Elixir"
                        $ add "F#"
                        $ add "Erlang"
                        $ add "Haskell"
                        $ add "Clojure" new
        updatedList `shouldBe` ["Elixir", "F#", "Erlang", "Haskell", "Clojure"]

    it "add then remove results in empty list" $ do
        let updatedList = remove
                        $ add "Haskell" new
        updatedList `shouldBe` []

    it "adding two languages, when removed, removes first item" $ do
        let updatedList = remove
                        $ add "Elixir"
                        $ add "Haskell" new
        updatedList `shouldBe` ["Haskell"]

    it  "add one language, then get the first" $ do
        let firstElement = first
                        $ add "Haskell" new
        firstElement `shouldBe` "Haskell"

    it  "add one language, then get the first" $ do
        let firstElement = first
                        $ add "F#"
                        $ add "Erlang"
                        $ add "Elixir"
                        $ add "Haskell" new
        firstElement `shouldBe` "F#"

    it  "the count of a new list is 0" $ do
        count new `shouldBe` 0

    it  "the count of a one-language list is 1" $ do
        let countElements = count
                $ add "Haskell" new  
        countElements  `shouldBe` 1

    it "the count of a multiple-item list is equal to its length" $ do
        let countElements = count
                $ add "Erlang"
                $ add "Elixir"
                $ add "Haskell" new  
        countElements  `shouldBe` 3

    it "a functional language list" $ do
        isFunctionalList ["Clojure", "Haskell", "Erlang", "Elixir", "F#"] `shouldBe` True

    it "not a functional language list" $ do
        isFunctionalList ["Java", "C", "JavaScript"] `shouldBe` False
