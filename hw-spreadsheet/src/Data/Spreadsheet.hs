{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Spreadsheet where

import Control.Monad.Except (MonadError)
import Control.Monad.Free (Free)
import Data.Array (Array)

-- | A spreadsheet cell can be indexed by pair of @Int@s.
type Index = (Int, Int)

-- | A spreadsheet is a 2-dimensional array of possibly empty cells.
newtype Spreadsheet a = Spreadsheet
  { getSpreadsheet :: Array Index (Maybe a)
  }
  deriving (Functor)

-- (1 балл) Сделайте @Spreadsheet a@ представителем класса @Show@.
-- Чтобы вывести двумерную таблицу с выравниванием, можно воспользоваться
-- арсеналом библиотеки @pretty@.

instance Show a => Show (Spreadsheet a)

-- (1 балл) Напишите вспомогательные функции для манипуляции таблицами.

-- | Appends new, empty row to the bottom of a spreadsheet
appendRow :: Spreadsheet a -> Spreadsheet a
appendRow = error "appendRow is not defined"

-- | Appends new, empty column to the right of a spreadsheet
appendCol :: Spreadsheet a -> Spreadsheet a
appendCol = error "appendCol is not defined"

-- | Returns cell in a spreadsheet by index
(!?) :: Spreadsheet a -> Index -> Maybe a
(!?) = error "spreadsheet index is not defined"

-- | A @Formula f a@ in a spreadsheet cell can:
--
--    * reference results of other cells by index;
--
--    * use constants of type @a@;
--
--    * perform operations listed in grammar @f@.
type Formula f a = Free f (Either Index a)

-- | A type of errors which can happen during cell evaluation.
data EvalError
  = -- | Cell refers to an empty cell.
    BlankReferee
  | -- | There is a dependency cycle between cells.
    RecursionLimitExceeded
  deriving (Show)

-- (1.5 балла) Используя @ffix@ для функтора @Spreadsheet@, напишите функцию
-- @calc@, вычисляющую значения в клетках таблицы. @evalExpression@ и
-- @prepareExpression@ даны в качестве подсказок для разбиения функции на этапы.
--
-- NB: для того, чтобы @ffix@ завершился, нужно, чтобы в зависимостях клеток
-- друг от друга не было циклов. Мы не проверяем ацикличность графа
-- зависимостей, а просто ограничиваем глубину строящихся выражений с помошью
-- параметра типа @Integer@. Для этого есть готовый комбинатор в модуле
-- @Control.Monad.Free@, найдите его.

-- | A function to compute values in a spreadsheet.
calc ::
  (Traversable f, MonadError EvalError m) =>
  -- | How to perform operations listed in grammar @f@.
  (f (m a) -> m a) ->
  -- | Recursion limit.
  Integer ->
  -- | Spreadsheet of formulae.
  Spreadsheet (Formula f a) ->
  Spreadsheet (m a)
calc alg limit = error "calc is not defined"
  where
    evalExpression ::
      (Traversable f, MonadError EvalError m) =>
      (f (m a) -> m a) ->
      Integer ->
      Free f (m a) ->
      m a
    evalExpression alg limit = error "evalExpression is not defined"

    prepareExpression ::
      (Functor f, MonadError EvalError m) =>
      Spreadsheet (Free f (m a)) ->
      -- \^ Imagine that a spreadsheet of expressions @s@ is ready...
      Formula f a ->
      -- \^ And you are given a formula @f@ which may refer to cells in @s@.
      Free f (m a)
    -- \^ How do you substitute expressions in @f@
    -- to create another expression?
    prepareExpression s f = error "prepareExpression is not defined"
