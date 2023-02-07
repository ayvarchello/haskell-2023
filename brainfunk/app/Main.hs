module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Lib

main :: IO ()
main = do
  strandLen <- readLn
  putStrLn ("strand length: " <> show strandLen)
  state <- makeInterpreter strandLen
  runReaderT (forever run) state

run :: ReaderT InterpreterState IO ()
run = do
  program <- liftIO getLine
  runStateInterpreter (runProgram $ parseProgram program)
  state <- ask
  liftIO (printInterpreter state)
