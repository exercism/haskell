import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad      (replicateM, void)
import Data.Foldable      (for_)
import Data.Traversable   (for)
import Data.Maybe         (isNothing, catMaybes)
import Test.Hspec         (Spec, it, shouldReturn)
import Test.Hspec.Runner  (configFailFast, defaultConfig, hspecWith)

import BankAccount
  ( closeAccount
  , incrementBalance
  , getBalance
  , openAccount
  )

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do

    it "checks that a new account is initialized to 0" $ do
        account  <-  openAccount
        getBalance   account `shouldReturn` Just 0
        closeAccount account

    it "sequentially increments an account then checks the balance" $ do
        account    <-    openAccount
        getBalance       account    `shouldReturn` Just  0
        incrementBalance account 10 `shouldReturn` Just 10
        incrementBalance account 20 `shouldReturn` Just 30
        getBalance       account    `shouldReturn` Just 30
        closeAccount     account

    it "concurrently increments an account then checks the balance" $ do
        let nThreads = 50
        account <- openAccount
        vs      <- replicateM nThreads $ do
          v <- newEmptyMVar
          void $ forkIO $ do
            void $ incrementBalance account 1
            putMVar v ()
          return v
        for_ (reverse vs) takeMVar
        getBalance   account `shouldReturn` Just (toInteger nThreads)
        closeAccount account

    it "concurrently increments an account AND checks the balance" $ do
        let nThreads = 50
        account <- openAccount
        vs      <- replicateM nThreads $ do
          v <- newEmptyMVar
          void $ forkIO $ do
            i <- incrementBalance account 1
            b <- getBalance account
            putMVar v [ if isNothing i then Just "Failed to increment!"   else Nothing
                      , if isNothing b then Just "Failed to get balance!" else Nothing
                      ]
          return v
        fmap (catMaybes . concat) (for vs takeMVar) `shouldReturn` []
        getBalance   account `shouldReturn` Just (toInteger nThreads)
        closeAccount account

    it "checks that a closed account holds nothing" $ do
        account    <-    openAccount
        getBalance       account    `shouldReturn` Just  0
        incrementBalance account 15 `shouldReturn` Just 15
        closeAccount     account
        getBalance       account    `shouldReturn` Nothing
        incrementBalance account 10 `shouldReturn` Nothing
