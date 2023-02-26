{-# LANGUAGE TemplateHaskell #-}

import Bank.Pure (PureBank)
import Control.Monad (unless)
import Numeric.Natural (Natural)
import System.Exit (exitFailure)
import Test.QuickCheck (quickCheckAll)

-- (1.5 балла) Выпишите сформулированные Вами законы @MonadBank@ в виде свойств
-- QuickCheck и проверьте, что они выполняются для @PureBank@.
--
-- В качестве типа для баланса предлагается использовать тип @Natural@.
--
-- Замечание: обратите внимание, что генератор QuickCheck по умолчанию будет
-- генерировать очень много несуществующих аккаунтов. Подумайте, как это обойти.

type TestBank = PureBank Natural

return []

main = do
  success <- $quickCheckAll
  unless success exitFailure
