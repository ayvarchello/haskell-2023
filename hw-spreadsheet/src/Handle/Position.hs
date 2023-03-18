module Handle.Position where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Direction (Direction)
import Data.IORef (IORef)
import Data.Spreadsheet (Index)

newtype PositionHandle = PositionHandle {getPositionHandle :: IORef Index}

class HasPosition e where
  position :: e -> PositionHandle

instance HasPosition PositionHandle where
  position = id

-- (0.5 балла) Реализуйте интерфейс курсора таблицы.

newPositionHandle :: Index -> IO PositionHandle
newPositionHandle = error "newPositionHandle is not defined"

getPosition :: (HasPosition e, MonadReader e m, MonadIO m) => m Index
getPosition = error "getPosition is not defined"

movePosition :: (HasPosition e, MonadReader e m, MonadIO m) => Direction -> m ()
movePosition d = error "movePosition is not defined"
