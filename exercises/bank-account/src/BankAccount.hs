module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

data BankAccount = Dummy

closeAccount :: BankAccount -> IO ()
closeAccount = error "You need to implement this function."

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = error "You need to implement this function."

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance = error "You need to implement this function."

openAccount :: IO BankAccount
openAccount = error "You need to implement this function."
