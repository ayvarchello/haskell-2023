{-# LANGUAGE FlexibleContexts #-}

module AppHandle (AppEnv, newAppEnv, run) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.Array (Array, array, (!), (//))
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (hFlush, stdout)

data AppEnv = Env
  { prompt :: !String,
    histHandle :: !HistoryHandle,
    cacheHandle :: !CacheHandle
  }

instance HasHistory AppEnv where
  historyHandle = histHandle

instance HasCache AppEnv where
  cache = cacheHandle

newAppEnv :: String -> IO AppEnv
newAppEnv prompt = Env prompt <$> newHistoryHandle <*> newCacheHandle

-- ReaderT pattern
-- RIO

run :: (MonadReader AppEnv m, MonadIO m) => m ()
run = do
  prompt <- asks prompt
  liftIO (putStr prompt >> hFlush stdout)
  input <- liftIO getLine
  appendHistory input

-- Handle pattern
newtype HistoryHandle = HistoryHandle {history :: IORef [String]}

-- Has pattern
class HasHistory env where
  historyHandle :: env -> HistoryHandle

instance HasHistory HistoryHandle where
  historyHandle = id

newHistoryHandle :: IO HistoryHandle
newHistoryHandle = HistoryHandle <$> newIORef []

appendHistory ::
  (MonadReader env m, HasHistory env, MonadIO m) => String -> m ()
appendHistory input = do
  historyHandle <- asks (history . historyHandle)
  hist <- liftIO $ readIORef historyHandle
  liftIO $ writeIORef historyHandle (input : hist)

newtype CacheHandle = CacheHandle {bits :: IOArray Int Bool}

class HasCache env where
  cache :: env -> CacheHandle

instance HasCache CacheHandle where
  cache = id

newCacheHandle :: IO CacheHandle
newCacheHandle = CacheHandle <$> newArray (0, 99) False

flipBit :: (MonadReader env m, HasCache env, MonadIO m) => Int -> m ()
flipBit k = do
  ref <- asks (bits . cache)
  bit <- liftIO (readArray ref k)
  liftIO $ writeArray ref k (not bit)
