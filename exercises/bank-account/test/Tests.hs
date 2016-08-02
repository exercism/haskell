import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad      (replicateM)
import Data.Foldable      (for_)
import Test.Hspec         (Spec, describe, it, shouldReturn)
import Test.Hspec.Runner  (configFastFail, defaultConfig, hspecWith)

import BankAccount
  ( closeAccount
  , incrementBalance
  , getBalance
  , openAccount
  )

{-
The BankAccount module should support four calls:

openAccount
  Called at the start of each test. Returns a BankAccount.

closeAccount account
  Called at the end of each test.

getBalance account
  Get the balance of the bank account.

updateBalance account amount
  Increment the balance of the bank account by the given amount.
  The amount may be negative for a withdrawal.

The initial balance of the bank account should be 0.
-}

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "bank-account" $ do

    -- As of 2016-08-01, there was no reference file
    -- for the test cases in `exercism/x-common`.

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
        incrementBalance account 10 `shouldReturn` Just 10
        closeAccount     account
        getBalance       account    `shouldReturn` Nothing
        incrementBalance account 10 `shouldReturn` Nothing
