module Main (main) where

import Control.Exception (bracket)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (ReaderT, runReaderT), forM_)
import Control.Monad.Trans.Resource (MonadResource, runResourceT, allocate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef (IORef)
import qualified Data.IORef as R
import qualified System.IO as IO

getFileContents :: FilePath -> IO ByteString
getFileContents path = IO.withBinaryFile path IO.ReadMode $ \h ->
  let loop accum = do
        buf <- B.hGetSome h 4096
        if B.null buf
          then return accum
          else loop (accum <> buf)
   in loop mempty

fileLen :: FilePath -> IO Int
fileLen file = B.length <$> getFileContents file

myFileLen :: FilePath -> IO Int
myFileLen path = IO.withBinaryFile path IO.ReadMode $ \h ->
  let loop accum = do
        buf <- B.hGetSome h 4096
        if B.null buf
          then return accum
          else loop (accum + B.length buf)
   in loop 0

data ListT m a = LNil | LCons a (m (ListT m a))

getFileStream :: MonadIO m => IO.Handle -> m (ListT m ByteString)
getFileStream h = do
  buf <- liftIO (B.hGetSome h 4096)
  if B.null buf
    then liftIO (IO.hClose h) >> return LNil
    else return $ LCons buf (getFileStream h)

streamConcat ::
  Monad m => m (ListT m a) -> m (ListT m a) -> m (ListT m a)
streamConcat left right = do
  l' <- left
  case l' of
    LNil -> right
    LCons buf tail -> return $ LCons buf (streamConcat tail right)

streamLen :: Monad m => m (ListT m ByteString) -> m Int
streamLen = helper 0
  where
    helper acc stream = do
      s <- stream
      case s of
        LNil -> return acc
        LCons buf tail -> helper (acc + B.length buf) tail

allocHandle :: MonadResource m => FilePath -> m IO.Handle
allocHandle file =
  snd <$> allocate (IO.openBinaryFile file IO.ReadMode) IO.hClose

stream file = allocHandle file >>= getFileStream

main :: IO ()
main = do
  file1 <- getLine
  file2 <- getLine
  len <- runResourceT $ do
    streamLen $ stream file1 `streamConcat` stream file2
  print len
