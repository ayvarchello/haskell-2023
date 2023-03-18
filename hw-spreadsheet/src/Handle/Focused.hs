{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Handle.Focused where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Direction (Direction)
import Data.Spreadsheet (EvalError, Index, Spreadsheet)
import Handle.Position (HasPosition, PositionHandle)
import qualified Handle.Position as P
import Handle.Spreadsheet (Cell, HasSpreadsheet (..), SpreadsheetHandle, Value)
import qualified Handle.Spreadsheet as S

data FocusedHandle f = FocusedHandle
  { positionHandle :: PositionHandle,
    spreadsheetHandle :: SpreadsheetHandle f
  }

class (HasPosition e, HasSpreadsheet f e) => HasFocused f e | e -> f where
  focused :: e -> FocusedHandle f

instance HasFocused f (FocusedHandle f) where
  focused = id

instance HasPosition (FocusedHandle f) where
  position = positionHandle

instance HasSpreadsheet f (FocusedHandle f) where
  spreadsheet = spreadsheetHandle

-- (1 балл) Релизуйте интерфейс связки курсор + таблица, где таблица
-- автоматически расширяется по надобности.

newFocusedHandle :: Index -> IO (FocusedHandle f)
newFocusedHandle index = error "newFocusedHandle is not defined"

getPosition :: (HasFocused f e, MonadReader e m, MonadIO m) => m Index
getPosition = error "getPosition is not defined"

currentCell ::
  (HasFocused f e, MonadReader e m, MonadIO m) => m (Maybe (Cell f))
currentCell = error "currentCell is not defined"

writeCurrent ::
  (HasFocused f e, MonadReader e m, MonadIO m) => Maybe (Cell f) -> m ()
writeCurrent cell = error "writeCurrent is not defined"

movePosition ::
  (HasFocused f e, MonadReader e m, MonadIO m) => Direction -> m ()
movePosition dir = error "movePosition is not defined"

calc ::
  ( Traversable f,
    HasFocused f e,
    MonadError EvalError n,
    MonadReader e m,
    MonadIO m
  ) =>
  (f (n Value) -> n Value) ->
  Integer ->
  m (Spreadsheet (n Value))
calc = error "calc is not defined"
