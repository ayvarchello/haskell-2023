{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ReaderT)
import Data.Functor.Classes (Read1, Show1)
import Data.Spreadsheet (EvalError)
import Handle.App (AppHandle)
import Handle.Spreadsheet (Value)

data Mode = MNum | MAll deriving (Read)

-- (0.5 балла) Реализуйте REPL, до бесконечности повторяющий чтение очередной
-- команды, её обработку и вывод результата.

repl ::
  (Read1 f, Show1 f, Traversable f, MonadError EvalError m, Show (m Value)) =>
  ReaderT (AppHandle f m) IO ()
repl = error "repl is not defined"

-- (0.5 балла) Реализуйте @main@, корректно обрабатывающий аргументы командной
-- строки и запускающий @repl@ с правильным @AppHandle@.

main :: IO ()
main = putStrLn "Hello, world!"
