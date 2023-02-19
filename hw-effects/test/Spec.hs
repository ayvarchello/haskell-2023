{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Bank.Pure (PureBank)
import System.Exit (exitFailure)
import Test.QuickCheck (quickCheckAll)

-- (1.5 балла) Выпишите сформулированные Вами законы @MonadBank@ в виде свойств
-- QuickCheck и проверьте, что они выполняются для @PureBank@.
--
-- В качестве типа для баланса предлагается использовать тип @Integer@.
--
-- Замечание: обратите внимание, что генератор QuickCheck по умолчанию будет
-- генерировать очень много несуществующих аккаунтов. Подумайте, как это обойти.

type TestBank = PureBank Integer

return []

main = do
  success <- $quickCheckAll
  unless success exitFailure
