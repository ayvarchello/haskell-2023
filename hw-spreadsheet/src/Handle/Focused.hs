{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Handle.Focused where

import Control.Monad(when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)

import Handle.Position (HasPosition, PositionHandle, newPositionHandle)
import qualified Handle.Position as P
import Handle.Spreadsheet (Cell, HasSpreadsheet (..), SpreadsheetHandle, Value, newSpreadsheetHandle)
import qualified Handle.Spreadsheet as S

import Data.Array (Ix(index), Array)
import Data.IORef(readIORef, writeIORef)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.Direction (Direction)
import Data.Spreadsheet (EvalError, Index, Spreadsheet, emptySS)


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
newFocusedHandle index = do
  pos <- newPositionHandle index
  ss <- newSpreadsheetHandle emptySS
  pure $ FocusedHandle pos ss

getPosition :: (HasFocused f e, MonadReader e m, MonadIO m) => m Index
getPosition = P.getPosition

currentCell ::
    (HasFocused f e, MonadReader e m, MonadIO m) => m (Maybe (Cell f))
currentCell = do
  pos <- getPosition
  S.readCell pos

writeCurrent ::
  (HasFocused f e, MonadReader e m, MonadIO m) => Maybe (Cell f) -> m ()
writeCurrent cell = do
  pos <- getPosition
  S.writeCell pos cell

movePosition ::
  (HasFocused f e, MonadReader e m, MonadIO m) => Direction -> m ()
movePosition dir = do
  P.movePosition dir
  pos <- getPosition
  bounds <- S.bounds
  when (snd pos > snd (snd bounds)) S.appendCol
  when (fst pos > fst (snd bounds)) S.appendRow 

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
calc = S.calcSpreadsheet
