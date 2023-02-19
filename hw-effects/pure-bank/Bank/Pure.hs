{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bank.Pure where

import Bank (MonadBank (..))
import Data.Accounts (Account, Accounts)
import Data.Monoid (Sum)

-- (0.5 балла) исправьте тип поля @runPureBank@, чтобы для @PureBank@ можно было
-- выписать инстанс @MonadBank@, для которого выполнены сформулированные Вами
-- законы. Рекомендуется использовать трансформеры монад.

-- | Monad for pure bank computations. @b@ is a balance type,
-- @a@ is a computation result type.
newtype PureBank b a = PureBank {runPureBank :: Accounts b -> a}

-- (0.5 балла) сделайте @PureBank b@ монадой. Если в предыдущем задании Вы
-- использовали трансформеры, рекомендуется воспользоваться командой
-- @deriving newtype@.

instance Functor (PureBank b)

instance Applicative (PureBank b)

instance Monad (PureBank b)

-- (1 балл) сделайте @PureBank b@ представителем класса @MonadBank@. Заголовок
-- инстанса менять запрещается. Рекомендуется использовать вспомогательные
-- чистые функции, реализованные ранее.

instance (Num b, Ord b) => MonadBank Account (Sum b) (PureBank b)
