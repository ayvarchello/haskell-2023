module Bank.Handle where

import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Numeric.Natural (Natural)

type Account = Int

type Balance = Natural

newtype BankHandle = BankHandle {getBankStorage :: IORef (IntMap Balance)}

-- (1.5 балла) реализуйте интерфейс @BankHandle@. Не забудьте, что для какой-то
-- монады должен быть реализован инстанс @MonadBank@.
--
-- Используйте паттерны Handle и RIO.
