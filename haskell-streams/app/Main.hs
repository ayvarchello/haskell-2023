module Main where

import Conduit
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import System.IO (openBinaryFile, IOMode (ReadMode), hClose)

magic :: Int -> IO Int
magic x = do
  putStrLn ("I'm doing magic with " ++ show x)
  return (x * 2)

listEx :: IO ()
listEx = mapM_ print =<< mapM magic (take 10 [1 ..])

conduitEx :: IO ()
conduitEx =
  runConduit $ yieldMany [1 ..] .| takeC 10 .| mapMC magic .| mapM_C print

parallelExample :: Monad m => ConduitT i Int m ()
parallelExample = do
  yieldMany [1 .. 10]
  yieldMany [2 .. 8]

complicatedMap :: Monad m => ConduitT Int Int m ()
complicatedMap = do
  takeC 5 .| mapC (* 2)
  mapC (+ 1)

condResults :: Monad m => ConduitT Int o m (String, Int)
condResults = do
  x <- takeC 5 .| mapC show .| foldC
  y <- sumC
  return (x, y)

primitives :: Monad m => o -> (i -> o) -> ConduitT i o m ()
primitives x f = do
  yield x
  y <- await
  case y of
    Just y -> do
      yield (f y)
      leftover y
    Nothing -> return ()

peek :: Monad m => ConduitT i o m (Maybe i)
peek = do
  x <- await
  maybe (return ()) leftover x
  return x

files1 = withSourceFile "tmp" $ \c -> runConduit (c .| foldC) :: IO ByteString

files2 :: MonadResource m => ConduitT i ByteString m ()
files2 = bracketP (openBinaryFile "tmp" ReadMode) hClose sourceHandle

files3 :: MonadResource m => ConduitT i ByteString m ()
files3 = sourceFile "tmp"

main :: IO ()
main = do
  putStrLn "List example:"
  listEx
  putStrLn "Conduit example:"
  conduitEx
  putStrLn "More examples:"
  print . runConduitPure $ parallelExample .| complicatedMap .| condResults
  print . runConduitPure $ return () .| (forM_ [1 .. 10] leftover >> sinkList)
  runConduitRes $ (sourceFile "input.txt" >> sourceFile "input2.txt") .| sinkFile "output.txt"
