{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Handle.App where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.AppView (AppView)
import Data.Direction (Direction)
import Data.Spreadsheet (EvalError, Index, Spreadsheet)
import Handle.Focused (FocusedHandle, HasFocused (..))
import qualified Handle.Focused as F
import Handle.Position (HasPosition (..))
import Handle.Spreadsheet (Cell, HasSpreadsheet (..), Value)

data AppHandle f m = AppHandle
  { algebra :: f (m Value) -> m Value,
    recursionLimit :: Integer,
    focusedHandle :: FocusedHandle f
  }

class HasFocused f e => HasApp f m e | e -> f, e -> m where
  app :: e -> AppHandle f m

instance HasApp f m (AppHandle f m) where
  app = id

instance HasPosition (AppHandle f m) where
  position = position . focused

instance HasSpreadsheet f (AppHandle f m) where
  spreadsheet = spreadsheet . focused

instance HasFocused f (AppHandle f m) where
  focused = focusedHandle

-- (0.5 балла) Реализуйте интерфейс приложения.

newAppHandle ::
  (f (m Value) -> m Value) -> Integer -> Index -> IO (AppHandle f m)
newAppHandle alg limit index = error "newAppHandle is not defined"

movePosition :: (HasApp f n e, MonadReader e m, MonadIO m) => Direction -> m ()
movePosition = error "movePosition is not defined"

writeCurrent ::
  (HasApp f n e, MonadReader e m, MonadIO m) => Maybe (Cell f) -> m ()
writeCurrent = error "writeCurrent is not defined"

view ::
  ( HasApp f n e,
    Traversable f,
    MonadError EvalError n,
    MonadReader e m,
    MonadIO m
  ) =>
  m (AppView f n Value)
view = error "view is not defined"
