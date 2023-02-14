module Main where

import Control.Concurrent (forkIO, getChanContents, newChan, newMVar, putMVar, readChan, takeMVar, threadDelay, writeChan)
import Control.Exception (handle)
import Control.Monad (forM, forM_, replicateM, replicateM_)
import Control.Parallel (par)
import Data.Foldable (traverse_)
import Data.List (zip4)
import Text.Printf (printf)

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = left `par` left + right
  where
    left = fib (n - 1)
    right = fib (n - 2)

newWorker :: IO a -> IO a
newWorker work = do
  mvar <- newMVar undefined
  takeMVar mvar
  forkIO $ do
    result <- work
    putMVar mvar result
  takeMVar mvar

timeSort :: [Int] -> IO [Int]
timeSort xs = do
  chan <- newChan
  forM_ xs $ \i -> forkIO $ do
    threadDelay (i * 100)
    writeChan chan i
  take (length xs) <$> getChanContents chan

diningPhilosophers :: Int -> Int -> IO ()
diningPhilosophers m n = do
  locks <- replicateM n (newMVar ())
  handles <- replicateM n (newMVar ())
  let philosophers = zip4 [0 ..] handles locks (tail locks ++ [head locks])
  forM_ handles takeMVar
  forM_ philosophers $ \(i, handle, left, right) ->
    forkIO $ do
      forM_ [0 .. m] $ \j -> do
        takeMVar left
        takeMVar right
        print (i, j)
        putMVar left ()
        putMVar right ()
      putMVar handle ()
  forM_ handles takeMVar

-- Software transactional memory

main :: IO ()
main = do
  -- input <- getContents
  -- print (take 10 input)
  -- getLine
  -- forM_ [0..100] $ \i -> forkIO $ do
  --  putStrLn $ concatMap show [0..100]
  --  putStrLn "Hello world!"
  -- putStrLn "Hi to the other thread!"
  -- result <- forM [0..29] $ \i -> newWorker $ do
  --   let result = fib i
  --   return result
  -- print result
  -- sorted <- timeSort [23423, 1, 24234, 5, 123]
  -- forM_ sorted print
  -- forM_ [0 .. 45] $ \i -> printf "n = %d => %d\n" i (fib i)
  n <- readLn
  m <- readLn
  diningPhilosophers m n
