{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks)
import Data.Array.IO (IOArray, MArray (newArray), readArray, writeArray, getElems)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Word (Word8)
import Data.Foldable (traverse_)

class Monad m => MonadInterpreter m where
  plus :: m ()
  loop :: m () -> m ()
  left :: m ()
  right :: m ()
  input :: m ()
  output :: m ()

data InterpreterState = MkState
  { pointer :: !(IORef Integer),
    memory :: !(IOArray Integer Word8)
  }

printInterpreter :: InterpreterState -> IO ()
printInterpreter (MkState ptr memory) = do
  readIORef ptr >>= print
  getElems memory >>= print

class HasInterpreterState env where
  interpreterState :: env -> InterpreterState

instance HasInterpreterState InterpreterState where
  interpreterState = id

makeInterpreter :: Integer -> IO InterpreterState
makeInterpreter strandLen = do
  pointer <- newIORef 0
  memory <- newArray (0, strandLen - 1) (toEnum 0)
  return $ MkState pointer memory

newtype StateInterpreter e m a = StateInterpreter {runStateInterpreter :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  (Monad m, MonadReader env m, HasInterpreterState env, MonadIO m) =>
  MonadInterpreter (StateInterpreter env m)
  where
  plus = StateInterpreter $ do
    ref <- asks (pointer . interpreterState)
    position <- liftIO $ readIORef ref
    array <- asks (memory . interpreterState)
    value <- liftIO $ readArray array position
    liftIO $ writeArray array position (succ value)
  loop = error "TODO"
  left = error "TODO"
  right = error "TODO"
  input = error "TODO"
  output = error "TODO"

data Command = CPlus | CLoop !Program | CLeft | CRight | CInput | COutput

type Program = [Command]

parseProgram :: String -> Program
parseProgram = error "TODO"

runCommand :: MonadInterpreter m => Command -> m ()
runCommand CPlus = plus
runCommand (CLoop coms) = loop (runProgram coms)
runCommand CLeft = left
runCommand CRight = right
runCommand CInput = input
runCommand COutput = output

runProgram :: MonadInterpreter m => Program -> m ()
runProgram = traverse_ runCommand
