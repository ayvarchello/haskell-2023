module Lib where

import Data.NonNegative (NonNegative (..))

factorial :: NonNegative -> Int
-- factorial n = if getNonNegative n >= 0 then _ else _
factorial n = factorialTR 1 (getNonNegative n)
  where
    factorialTR = \r n -> if n == 0 then r else let rn = r * n in factorialTR rn (n - 1)
