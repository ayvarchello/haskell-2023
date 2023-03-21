module MVars where

import Control.Concurrent (MVar, modifyMVar, newMVar)

type Account = MVar Int

new :: IO Account
new = newMVar 0

withdraw :: Int -> Account -> IO ()
withdraw n account = modifyMVar account (\balance -> return (balance - n, ()))

deposit :: Int -> Account -> IO ()
deposit n = withdraw (-n)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount = withdraw amount from >> deposit amount to

transfer' from to amount = modifyMVar from $
  \fromBalance -> modifyMVar to $
    \toBalance -> return (toBalance + amount, (fromBalance - amount, ()))
