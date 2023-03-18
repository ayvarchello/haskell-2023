{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Handle.Spreadsheet where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Array.IO (IOArray)
import Data.IORef (IORef)
import Data.Spreadsheet (EvalError, Formula, Index, Spreadsheet)
import qualified Data.Spreadsheet as S

type Value = Rational

type Cell f = Formula f Value

newtype SpreadsheetHandle f = SpreadsheetHandle
  {getSpreadsheetHandle :: IORef (IOArray Index (Maybe (Cell f)))}

class HasSpreadsheet f e | e -> f where
  spreadsheet :: e -> SpreadsheetHandle f

instance HasSpreadsheet f (SpreadsheetHandle f) where
  spreadsheet = id

-- (1 балл) Реализуйте интерфейс расширяемой таблицы.

newSpreadsheetHandle :: Spreadsheet (Cell f) -> IO (SpreadsheetHandle f)
newSpreadsheetHandle = error "newSpreadsheetHandle is not defined"

readCell ::
  (HasSpreadsheet f e, MonadReader e m, MonadIO m) =>
  Index ->
  m (Maybe (Cell f))
readCell index = error "readCell is not defined"

writeCell ::
  (HasSpreadsheet f e, MonadReader e m, MonadIO m) =>
  Index ->
  Maybe (Cell f) ->
  m ()
writeCell index cell = error "writeCell is not defined"

bounds :: (HasSpreadsheet f e, MonadReader e m, MonadIO m) => m (Index, Index)
bounds = error "bounds is not defined"

appendRow :: (HasSpreadsheet f e, MonadReader e m, MonadIO m) => m ()
appendRow = error "appendRow is not defined"

appendCol :: (HasSpreadsheet f e, MonadReader e m, MonadIO m) => m ()
appendCol = error "appendCol is not defined"

calcSpreadsheet ::
  ( HasSpreadsheet f e,
    Traversable f,
    MonadError EvalError n,
    MonadReader e m,
    MonadIO m
  ) =>
  (f (n Value) -> n Value) ->
  Integer ->
  m (Spreadsheet (n Value))
calcSpreadsheet alg limit = error "calcSpreadsheet is not defined"
