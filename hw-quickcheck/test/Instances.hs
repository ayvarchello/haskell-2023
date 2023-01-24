module Instances where

import Data.GroupExpr (GroupExpr, GroupLit)
import Data.Var (Var)
import Test.QuickCheck (Arbitrary)

-- (1 балл) Сделайте `Var`, `GroupLit` и `GroupExpr` представителями класса
-- Arbitrary.
--
-- Генератор `Var` должен выбирать равновероятно одну из 3-4 выбранных Вами
-- переменных, чтобы в получающемся слове часто встречались сократимые
-- фрагменты.
--
-- Случайный `GroupLit` должен быть `Direct` с вероятностью 75%, а `Invert` ---
-- с вероятностью 25% (для этого есть готовый комбинатор, найдите его).
--
-- Генератор `GroupExpr` должен быть разумным.

instance Arbitrary Var

instance Arbitrary a => Arbitrary (GroupLit a)

instance Arbitrary a => Arbitrary (GroupExpr a)
