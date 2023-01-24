{-# LANGUAGE DeriveFunctor #-}

module Data.GroupExpr where

import Data.Group (Group (..))

-- | @GroupLit a@ is a type of literals in arbitrary group with
-- variables ranging in @a@.
--
-- For example, a literal \(x\) would be encoded as @Direct \'x\'@, while a
-- literal \(y^{-1}\) would be encoded as @Invert \'y\'@.
data GroupLit a = Direct !a | Invert !a deriving (Show, Functor)

-- (0 баллов) Реализуйте более естественное и компактное строковое
-- представление для типа `GroupLit` вместо реализации по умолчанию.

-- | @GroupExpr a@ is a type of expressions in arbitrary group with
-- variables ranging in @a@.
--
-- For example, \(x y z y^{-1}\) would be encoded as
-- @GroupExpr [Direct \'x\', Direct \'y\', Direct \'z\', Invert \'y\']@.
newtype GroupExpr a = GroupExpr {getExpr :: [GroupLit a]}
  deriving (Show, Functor)

-- (1 балл) Сделайте `GroupExpr a` представителем класса `Group`.
-- Можно пользоваться дерайвингом; никаких условий на `a` быть не должно.

instance Semigroup (GroupExpr a)

instance Monoid (GroupExpr a)

instance Group (GroupExpr a)

-- (1 балл) Реализуйте вычисление выражения в группе.
-- (0 баллов) Говоря терминами из алгебры, чем является `groupEval v`?

groupEval ::
  Group g =>
  -- | Variable assignment.
  (x -> g) ->
  -- | Expression to evaluate.
  GroupExpr x ->
  -- | Result of evaluation.
  g
groupEval = error "`groupEval` is not implemented"

-- (1 балл) Сделайте `GroupExpr` представителем класса `Monad`.
-- Подсказка: у (>>=) есть реализация в одну строчку.

instance Applicative GroupExpr

instance Monad GroupExpr

-- (2.5 балла) Реализуйте проверку выражений на равенство с точностью до
-- сокращений:
-- `GroupExpr [Direct 'y', Direct 'x', Invert 'x'] == GroupExpr [Direct 'y']`
-- должно возвращать True.

instance Eq a => Eq (GroupExpr a)

-- (1 балл) Реализуйте более естественное и компактное строковое представление
-- для типа `GroupExpr a` вместо реализации по умолчанию.
