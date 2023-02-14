{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (Exception (..), SomeException (SomeException), catch, throwIO, try, mask)
import Data.Maybe (fromJust)
import Data.Typeable (cast)
import GHC.Stack (HasCallStack)
import System.IO (IOMode (ReadMode), hClose, hGetLine, hIsClosed, openFile, withFile)
import System.IO.Error (ioeGetHandle)

head' :: HasCallStack => [a] -> a
head' (x : _) = x
head' [] = error "empty list"

bottom :: a
bottom = undefined

secondOk :: HasCallStack => String
secondOk = error "bad operation"

data AppException = AppException | UnknownException SomeException deriving (Show)

instance Exception AppException

data CustomException = MyException deriving (Show)

instance Exception CustomException where
  fromException (SomeException e) = case cast e of
    Just (UnknownException e) -> cast e
    _ -> Nothing
  toException = toException . UnknownException . SomeException

main2 :: IO ()
main2 = do
  -- let (h : t) = ["hello world!", error "bad head"]
  -- print h
  -- print t
  -- print "Ok"
  -- print secondOk
  --   `catch` ( \e -> do
  --               putStr "Catched an exception: "
  --               print (e :: SomeException)
  -- error "bad catch"
  --           )
  -- print "Ok"

  mask $ \restore -> do
    file <- openFile "tmp" ReadMode
    restore $ do
    -- ASYNC EXCEPTION
      head <- hGetLine file
      print head
    hClose file

  file <-
    withFile
      "tmp"
      ReadMode
      ( \file -> do
          throwIO MyException
          head <- hGetLine file
          print head
          return file
      )
      `catch` (\(e :: IOError) -> pure $ fromJust $ ioeGetHandle e)
  isClosed <- hIsClosed file
  print isClosed

timeout :: Int -> IO a -> IO (Maybe a)
timeout ms act = do
  mvar <- newEmptyMVar
  threadId <- forkIO (act >>= putMVar mvar)
  forkIO $ do
    threadDelay ms
    killThread threadId
  result <- try (readMVar mvar)
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right result -> return (Just result)

main = do
  result <- timeout 100 (threadDelay 1000 >> pure "ok")
  print result
