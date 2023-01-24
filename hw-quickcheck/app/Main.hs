module Main where

import Data.GroupExpr (GroupExpr)

-- (0.5 балла) Реализуйте `substitute` с помощью `(>>=)`.

substitute ::
  Eq a =>
  -- | What to substitute.
  GroupExpr a ->
  -- | Which variable to substitute.
  a ->
  -- | Where to substitute.
  GroupExpr a ->
  -- | Result of a substitution
  GroupExpr a
substitute = error "Substitution is not defined"

-- (2 балла) Напишите программу, соответствующую описанию в README,
-- использующую `substitute` и `GroupExpr` для вычислений.

-- Возможно, для более дружелюбного вывода вам пригодится
-- тип данных `Var` из `src/Data/Var.hs`.

main :: IO ()
main = putStrLn "Hello, Haskell!"
