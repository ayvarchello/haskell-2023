module ListTVar where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import System.Random (getStdRandom, randomR)

proc :: TVar Int -> [TVar Int] -> IO ()
proc ix xs = do
  atomically $ do
    i <- readTVar ix
    writeTVar (xs !! i) (i + 1)
  putStrLn "ping!"

writer :: TVar Int -> [TVar Int] -> IO ()
writer ix xs = do
  rand <- getStdRandom $ randomR (0, length xs - 1)
  atomically (writeTVar ix rand)

example :: IO ()
example = do
  ix <- newTVarIO 0
  xs <- traverse newTVarIO [0 .. 15]
  forkIO $ forever (writer ix xs)
  forever (proc ix xs)
