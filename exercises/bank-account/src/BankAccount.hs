module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

closeAccount :: BankAccount -> IO ()
closeAccount = undefined

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = undefined

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance = undefined

openAccount :: IO BankAccount
openAccount = undefined
