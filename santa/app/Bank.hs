module Bank where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVarIO, orElse, readTVar, retry, writeTVar)

-- not IO, but STM

type Account = TVar Int

new :: IO Account
new = newTVarIO 0

withdraw_ :: Int -> Account -> STM ()
withdraw_ n account = modifyTVar account (\balance -> balance - n)

deposit n = withdraw_ (-n)

withdraw :: Int -> Account -> STM ()
withdraw n account = do
  balance <- readTVar account
  if balance >= n
    then writeTVar account (balance - n)
    else retry

or_else :: STM a -> STM a -> STM a
or_else = (<|>)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
  atomically $
    withdraw amount from >> deposit amount to

example :: IO ()
example = do
  from <- new
  to <- new
  transfer from to 1
