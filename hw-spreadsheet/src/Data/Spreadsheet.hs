{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Spreadsheet where

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Free (Free(Free, Pure))
import Data.Array (Array, assocs, bounds, array, (!))
import qualified Text.Tabl as TT
import qualified Data.Text as T
import Data.Functor.Fix(ffix)

-- | A spreadsheet cell can be indexed by pair of @Int@s.
type Index = (Int, Int)

-- | A spreadsheet is a 2-dimensional array of possibly empty cells.
newtype Spreadsheet a = Spreadsheet
  { getSpreadsheet :: Array Index (Maybe a)
  }
  deriving (Functor)

emptyArr :: Array Index (Maybe a)
emptyArr = array ((0, 0), (0, 0)) []

emptySS :: Spreadsheet a
emptySS = Spreadsheet emptyArr


-- (1 балл) Сделайте @Spreadsheet a@ представителем класса @Show@.
-- Чтобы вывести двумерную таблицу с выравниванием, можно воспользоваться
-- арсеналом библиотеки @pretty@

instance Show a => Show (Spreadsheet a) where
  show :: Show a => Spreadsheet a -> String
  show a = 
    let it ((rowIndex, _), elem) (currentRowIndex, chunks@(chunkHead: chunkTail)) = 
            if maybe True (rowIndex == ) currentRowIndex then 
              (Just rowIndex, (elem : chunkHead) : chunkTail) else 
              (Just rowIndex, [elem] : chunks) in
    let tData = snd $ foldr (it . (\(index, element) -> (index, T.pack $ maybe "*" show element))) (Nothing, [[]]) $ assocs $ getSpreadsheet a in
      T.unpack $ TT.tabl TT.EnvAscii (TT.DecorAll) (TT.DecorAll) (repeat TT.AlignLeft) tData

-- (1 балл) Напишите вспомогательные функции для манипуляции таблицами.

-- | Appends new, empty row to the bottom of a spreadsheet
appendRow :: Spreadsheet a -> Spreadsheet a
appendRow sheet = 
  let arr = getSpreadsheet sheet
      ((top, left), (bottom, right)) = bounds arr
      newRow = [((bottom + 1, j), Nothing) | j <- [left..right]]
      newBounds = ((top, left), (bottom + 1, right))
  in Spreadsheet $ array newBounds $ assocs arr ++ newRow

-- | Appends new, empty column to the right of a spreadsheet
-- | Adds a new column to the right of the spreadsheet.
appendCol :: Spreadsheet a -> Spreadsheet a
appendCol sheet =
    let arr = getSpreadsheet sheet
        ((top, left), (bottom, right)) = bounds arr
        newColumn = [((i, right + 1), Nothing) | i <- [top..bottom]]
        newBounds = ((top, left), (bottom, right + 1))
    in Spreadsheet $ array newBounds $ assocs arr ++ newColumn

-- | Returns cell in a spreadsheet by index
(!?) :: Spreadsheet a -> Index -> Maybe a
(!?) (Spreadsheet sheet) = (sheet !)

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

calc :: forall f m a.
  (Traversable f, MonadError EvalError m) =>
  -- | The function that performs operations on the given `Traversable` function `f`.
  (f (m a) -> m a) ->
  -- | The recursion limit.
  Integer ->
  -- | The spreadsheet of formulae.
  Spreadsheet (Formula f a) ->
  -- | The spreadsheet of computed expressions.
  Spreadsheet (m a)
calc alg limit = fmap (evalExpression alg limit) . ffix . fmap (flip prepareExpression)
  where
    evalExpression ::
      (Traversable f, MonadError EvalError m) =>
      (f (m a) -> m a) ->
      Integer ->
      Free f (m a) ->
      m a
    evalExpression _ _ (Pure x) = x
    evalExpression alg limit (Free v)
      | limit == 0 = throwError RecursionLimitExceeded
      | otherwise = alg $ (evalExpression alg (limit - 1)) <$> v

    prepareExpression ::
      (Functor f, MonadError EvalError m) =>
      Spreadsheet (Free f (m a)) ->
      -- \^ Imagine that a spreadsheet of expressions @s@ is ready...
      Formula f a ->
      -- \^ And you are given a formula @f@ which may refer to cells in @s@.
      Free f (m a)
    -- \^ How do you substitute expressions in @f@
    -- to create another expression?
    prepareExpression s f = f >>= \case 
                                    (Left idx) -> case s !? idx of 
                                      Nothing -> Pure $ throwError BlankReferee
                                      Just formula -> formula 
                                    (Right const) -> Pure $ pure const