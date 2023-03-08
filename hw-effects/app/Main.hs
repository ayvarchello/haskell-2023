module Main where

import Bank.Handle (Account, Balance, BankHandle)

data Input
  = INewAccount
  | IDeleteAccount Account
  | ITransfer Account Balance Account
  deriving (Read)

-- (0.5 балла) Реализуйте вспомогательные функции для взаимодействия с
-- пользователем.

parse :: IO Input
-- ^ Reads next command from standard input
parse = _

printStorage :: BankHandle -> IO ()
-- ^ Outputs bank storage contents
printStorage = _

-- (1 балл) Реализуйте взаимодействие с пользователем, использующее
-- вспомогательные функции выше и интерфейс BankHandle.
--
-- Рекомендуется использовать функцию @forever@ из модуля @Control.Monad@.

main :: IO ()
main = putStrLn "Hello, Haskell!"
