import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad      (replicateM)
import Data.Foldable      (for_)
import Test.Hspec         (Spec, it, shouldReturn)
import Test.Hspec.Runner  (configFastFail, defaultConfig, hspecWith)

import BankAccount
  ( closeAccount
  , incrementBalance
  , getBalance
  , openAccount
  )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

    it "initial balance is 0" $ do
        account  <-  openAccount
        getBalance   account `shouldReturn` Just 0
        closeAccount account

    it "incrementing and checking balance" $ do
        account    <-    openAccount
        getBalance       account    `shouldReturn` Just  0
        incrementBalance account 10 `shouldReturn` Just 10
        incrementBalance account 20 `shouldReturn` Just 30
        getBalance       account    `shouldReturn` Just 30
        closeAccount     account

    it "incrementing balance from other processes then checking it from test process" $ do
        account  <- openAccount
        vs <- replicateM 20 $ do
                  v <- newEmptyMVar
                  _ <- forkIO $ incrementBalance account 1 >> putMVar v ()
                  return v
        for_ vs takeMVar
        getBalance   account `shouldReturn` Just 20
        closeAccount account

    it "closed banks hold no balance" $ do
        account    <-    openAccount
        getBalance       account    `shouldReturn` Just  0
        incrementBalance account 15 `shouldReturn` Just 15
        closeAccount     account
        getBalance       account    `shouldReturn` Nothing
        incrementBalance account 10 `shouldReturn` Nothing
