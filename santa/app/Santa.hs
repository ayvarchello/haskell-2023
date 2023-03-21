{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Santa where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import System.Random

-- Santa
-- Reindeers 9/9
-- Elves 3/10

type Gate = (Int, TVar Int)

newGate :: Int -> STM Gate
newGate n = (n,) <$> newTVar 0

passGate :: Gate -> IO ()
passGate (_, gate) = atomically $ do
  left <- readTVar gate
  check (left > 0)
  writeTVar gate (left - 1)

raiseGate :: Gate -> IO ()
raiseGate (n, gate) = atomically $ do
  left <- readTVar gate
  check (left == 0)
  writeTVar gate n

type Group = (Int, TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically $ do
  in_gate <- newGate n
  out_gate <- newGate n
  (n,) <$> newTVar (0, in_gate, out_gate)

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (_, group) = atomically $ do
  (left, in_gate, out_gate) <- readTVar group
  check (left > 0)
  writeTVar group (left - 1, in_gate, out_gate)
  return (in_gate, out_gate)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (n, group) = do
  (left, in_gate, out_gate) <- readTVar group
  check (left == 0)
  newIn <- newGate n
  newOut <- newGate n
  writeTVar group (n, newIn, newOut)
  return (in_gate, out_gate)

randomDelay :: IO ()
randomDelay = do
  waitTime <- getStdRandom $ randomR (1, 1_000_000)
  threadDelay waitTime

type Elf = Int

elf1 :: Group -> Elf -> IO ()
elf1 group elf_id = do
  (in_gate, out_gate) <- joinGroup group
  passGate in_gate
  putStrLn ("Elf #" <> show elf_id <> " got to work")
  passGate out_gate

elf :: Group -> Elf -> IO ThreadId
elf group elf_id = forkIO $ forever (elf1 group elf_id >> randomDelay)

type Reindeer = Int

reindeer1 :: Group -> Reindeer -> IO ()
reindeer1 group reindeer_id = do
  (in_gate, out_gate) <- joinGroup group
  passGate in_gate
  putStrLn ("Reindeer #" <> show reindeer_id <> " is ready")
  passGate out_gate

reindeer :: Group -> Reindeer -> IO ThreadId
reindeer group reindeer_id =
  forkIO $ forever (reindeer1 group reindeer_id >> randomDelay)

santa :: Group -> Group -> IO ()
santa reindeers elves = do
  (in_gate, out_gate) <-
    atomically $ awaitGroup reindeers `orElse` awaitGroup elves
  putStrLn "Ho! Ho! Ho!"
  raiseGate in_gate
  raiseGate out_gate

example :: IO ()
example = do
  elfGroup <- newGroup 3
  elves <- traverse (elf elfGroup) [1 .. 10]

  reindeerGroup <- newGroup 9
  reindeers <- traverse (reindeer reindeerGroup) [1 .. 9]

  forever (santa reindeerGroup elfGroup)
