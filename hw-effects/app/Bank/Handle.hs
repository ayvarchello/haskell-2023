module Bank.Handle where

import Data.Array.IO (IOArray)
import Numeric.Natural (Natural)

type Account = Int

type Balance = Natural

newtype BankHandle = BankHandle {getBankStorage :: IOArray Account Balance}

-- (1.5 балла) реализуйте интерфейс @BankHandle@. Не забудьте, что для какой-то
-- монады должен быть реализован инстанс @MonadBank@.
--
-- Используйте паттерны Handle и RIO.
