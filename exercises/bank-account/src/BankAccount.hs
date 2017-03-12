module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

data BankAccount = Dummy

closeAccount :: BankAccount -> IO ()
closeAccount account = error "You need to implement this function."

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = error "You need to implement this function."

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = error "You need to implement this function."

openAccount :: IO BankAccount
openAccount = error "You need to implement this function."
