module Handle.Position where

import Control.Monad.Trans
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Direction (Direction, updatePosition)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Spreadsheet (Index)

newtype PositionHandle = PositionHandle {getPositionHandle :: IORef Index}

class HasPosition e where
  position :: e -> PositionHandle

instance HasPosition PositionHandle where
  position = id

-- (0.5 балла) Реализуйте интерфейс курсора таблицы.

newPositionHandle :: Index -> IO PositionHandle
newPositionHandle idx = do
  positionRef <- newIORef idx
  return $ PositionHandle positionRef

getPosition :: (HasPosition env, MonadReader env m, MonadIO m) => m Index
getPosition = do
  positionRef <- asks (getPositionHandle . position)
  liftIO $ readIORef positionRef

movePosition :: (HasPosition env, MonadReader env m, MonadIO m) => Direction -> m ()
movePosition d = do
  positionRef <- asks (getPositionHandle . position)
  liftIO $ modifyIORef positionRef (updatePosition d)
