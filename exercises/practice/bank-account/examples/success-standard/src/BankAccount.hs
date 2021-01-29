module BankAccount ( BankAccount
                   , openAccount, closeAccount
                   , getBalance, incrementBalance) where
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, readTVarIO, writeTVar)

newtype BankAccount = BankAccount { unBankAccount :: TVar (Maybe Integer) }

openAccount :: IO BankAccount
openAccount = atomically $ BankAccount <$> newTVar (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount = atomically . flip writeTVar Nothing . unBankAccount

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readTVarIO . unBankAccount

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance acct delta = atomically $ do
  let b = unBankAccount acct
  bal <- fmap (delta +) <$> readTVar b
  writeTVar b bal
  return bal
