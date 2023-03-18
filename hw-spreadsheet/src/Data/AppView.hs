module Data.AppView where

import Data.Functor.Classes (Show1)
import Data.Spreadsheet (Formula, Index, Spreadsheet)

data AppView f m a = AppView
  { position :: Index,
    formula :: Maybe (Formula f a),
    sheet :: Spreadsheet (m a)
  }
  deriving (Show)
