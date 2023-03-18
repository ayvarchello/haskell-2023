{-# LANGUAGE FlexibleContexts #-}

module Command where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.AppView (AppView)
import Data.Direction (Direction)
import Data.Functor.Classes (Read1)
import Data.Spreadsheet (EvalError, Formula)
import Handle.App (HasApp)
import Handle.Spreadsheet (Cell, Value)

data Command f = Move Direction | Input (Maybe (Cell f)) deriving (Read)

-- (0.5 балла) Реализуйте чтение и обработку команд, используя для обработки
-- интерфейс @AppHandle@.

nextCommand :: (MonadIO m, Read1 f) => m (Command f)
nextCommand = error "nextCommand is not defined"

runCommand ::
  ( HasApp f n e,
    Traversable f,
    MonadError EvalError n,
    MonadReader e m,
    MonadIO m
  ) =>
  Command f ->
  m (AppView f n Value)
runCommand = error "runCommand is not defined"
