{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Data.GroupExpr (GroupExpr)
import Data.Var (Var)
import Instances
import System.Exit (exitFailure)
import Test.QuickCheck

-- (0 баллов) Выполняется ли свойство `prop_minimum`? Проверьте.
-- (0.5 балла) Объясните.

prop_minimum = minimum(100, 500) === 500

-- (1 балл) Выпишите аксиомы групп в виде свойств QuickCheck и проверьте, что
-- они выполняются для Вашей реализации `GroupExpr`.

type TestExpr = GroupExpr Var

return []

main = do
  success <- $quickCheckAll
  unless success exitFailure
