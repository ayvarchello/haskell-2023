{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters where

import Control.Monad.Except (ExceptT, MonadError)
import Data.Functor.Dialects (Add, Div, Mul)
import Data.Spreadsheet (EvalError)
import Data.Functor.Sum (Sum)

infixr 5 :|:, ?:

type (f :|: g) = Sum f g

-- (0.5 балла) Реализуйте вспомогательную функцию для комбинации интерпретаторов.

(?:) :: (f a -> c) -> (g a -> c) -> (f :|: g) a -> c
(?:) = error "sum eliminator is not defined"

-- | A type of errors which can happen during @Div@ evaluation.
data DivError = DivByZero | DivEval EvalError deriving (Show)

-- (0.5 балла) Напишите интерпретаторы диалектов из модуля
-- @Data.Functor.Dialects@. @runDiv@ должен корректно обрабатывать случай
-- деления на 0.

runAdd :: (Num a, Applicative f) => Add (f a) -> f a
runAdd = error "runAdd is not defined"

runMul :: (Num a, Applicative f) => Mul (f a) -> f a
runMul = error "runMul is not defined"

runDiv :: (Fractional a, Eq a, MonadError DivError m) => Div (m a) -> m a
runDiv = error "runDiv is not defined"

-- (1 балл) Скомбинируйте интерпретаторы диалектов с помощью @(?:)@.
--
-- NB: @runDiv@ сам по себе скомбинировать не получится, нужна дополнительная
-- обвязка. Советую использовать @modifyError@ и @lift@.

type NumResult = Either EvalError

runNum :: Num a => (Add :|: Mul) (NumResult a) -> NumResult a
runNum = error "runNum is not defined"

type AllResult = ExceptT EvalError (Either DivError)

runAll ::
  (Fractional a, Eq a) => (Add :|: Mul :|: Div) (AllResult a) -> AllResult a
runAll = error "runAll is not defined"
