module Data.Accounts where

import Bank.Error (BankError)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- | Account type
type Account = Int

-- | Balance storage for pure computations
type Accounts = IntMap

-- | Result of operations
type Result = Either BankError

-- (1 балл) Реализуйте вспомогательные чистые функции @nextAccount@,
-- @getBalance@ и @modifyBalance@.
-- Функции из модуля @Data.IntMap.Strict@ можно вызывать с префиксом @M.@:
-- @
--    M.elems . M.delete 1 . M.insert 0 "wow"
-- @

newAccount :: Accounts b -> Account
-- ^ Finds fresh account name
newAccount = error "TODO"

balance :: Account -> Accounts b -> Result b
-- ^ Gets balance of an account, if it exists
balance = error "TODO"

modifyBalance ::
  -- | Account to modify balance for
  Account ->
  -- | Action to execute on its balance
  (b -> Result b) ->
  -- | Balance storage
  Accounts b ->
  -- | Result of action
  Result (Accounts b)
modifyBalance = error "TODO"
