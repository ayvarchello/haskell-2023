module Main (main) where

import AppHandle (newAppEnv, run)
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState (get), State, put, runState)

data AppState = AppState {sPrompt :: !String, sHistory :: ![String]}

appendHistoryState :: String -> State AppState ()
appendHistoryState input = do
  state <- get
  put (AppState (sPrompt state) (input : sHistory state))
  error "Exception"

-- catchError :: AppEnv -> IO ()
-- catchError env =
--   runReaderT (appendHistory "line") env `catch` _

-- catchErrorState :: AppState -> IO ()
-- catchErrorState env =
--   return (runState (appendHistoryState "line") env) `catch` _

main :: IO ()
main = newAppEnv "> " >>= runReaderT (forever run)
