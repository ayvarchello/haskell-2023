{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Handle.Spreadsheet where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Array.IO (IOArray)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Spreadsheet (EvalError, Formula, Index, Spreadsheet)
import qualified Data.Spreadsheet as P
import qualified Data.Array as A
import qualified Data.Spreadsheet as S
import qualified Data.Text as T
import GHC.IOArray (newIOArray, writeIOArray, readIOArray, boundsIOArray)
import Control.Arrow (Arrow(arr))
import Data.Foldable (for_)

type Value = Rational

type Cell f = Formula f Value

newtype SpreadsheetHandle f = SpreadsheetHandle
  {getSpreadsheetHandle :: IORef (IOArray Index (Maybe (Cell f)))}

class HasSpreadsheet f e | e -> f where
  spreadsheet :: e -> SpreadsheetHandle f

instance HasSpreadsheet f (SpreadsheetHandle f) where
  spreadsheet = id

-- (1 балл) Реализуйте интерфейс расширяемой таблицы.
  
readRaw :: IOArray Index (Maybe (Cell f)) -> IO (Spreadsheet (Cell f))
readRaw arr = P.Spreadsheet . A.listArray bounds <$> traverse (readIOArray arr) indices
  where
    bounds@((top, bottom), (left, right)) = boundsIOArray arr
    indices = [(i, j) | i <- [top..bottom], j <- [left..right]]

newSpreadsheetHandle :: Spreadsheet (Cell f) -> IO (SpreadsheetHandle f)
newSpreadsheetHandle sheet = do
  let bnds = A.bounds $ P.getSpreadsheet sheet
  arr <- newIOArray (bnds) Nothing 
  ref <- newIORef arr
  return $ SpreadsheetHandle ref

readCell ::
  (HasSpreadsheet f e, MonadReader e m, MonadIO m) =>
  Index ->
  m (Maybe (Cell f))
readCell index = do
  sh <- asks (getSpreadsheetHandle . spreadsheet)
  liftIO $ readIORef sh >>= \arr -> readIOArray arr index

writeCell ::
  (HasSpreadsheet f e, MonadReader e m, MonadIO m) =>
  Index ->
  Maybe (Cell f) ->
  m ()
writeCell index cell = do
  sh <- asks (getSpreadsheetHandle . spreadsheet)
  liftIO $ readIORef sh >>= \arr -> writeIOArray arr index cell

bounds :: (HasSpreadsheet f e, MonadReader e m, MonadIO m) => m (Index, Index)
bounds = do
  sh <- asks (getSpreadsheetHandle . spreadsheet)
  arr <- liftIO $ readIORef sh
  pure $ boundsIOArray arr

appendRow :: (HasSpreadsheet f e, MonadReader e m, MonadIO m) => m ()
appendRow = do
  spreadsheetHandle <- asks (getSpreadsheetHandle . spreadsheet)
  spreadsheetArray <- liftIO $ readIORef spreadsheetHandle
  spreadsheetData <- liftIO $ readRaw spreadsheetArray
  let newSpreadsheetData = P.getSpreadsheet (P.appendCol spreadsheetData)
  liftIO $ for_ (A.assocs newSpreadsheetData) $ \(idx, val) -> writeIOArray spreadsheetArray idx val

appendCol :: (HasSpreadsheet f e, MonadReader e m, MonadIO m) => m ()
appendCol = do
  spreadsheetHandle <- asks (getSpreadsheetHandle . spreadsheet)
  spreadsheetArray <- liftIO $ readIORef spreadsheetHandle
  spreadsheetData <- liftIO $ readRaw spreadsheetArray
  let newSpreadsheetData = P.appendCol spreadsheetData
  liftIO $ (for_ (A.assocs $ P.getSpreadsheet newSpreadsheetData) (uncurry $ writeIOArray spreadsheetArray))


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
calcSpreadsheet alg limit = do
  handle <- asks (getSpreadsheetHandle . spreadsheet)
  arr <- liftIO $ readIORef handle
  ss <- liftIO $ readRaw arr
  pure $ P.calc alg limit ss 
