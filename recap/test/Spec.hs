{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Data.NonNegative (NonNegative (..), nonNegative)
import Lib (factorial)
import System.Exit (exitFailure)
import Test.QuickCheck hiding (NonNegative (..))

instance Arbitrary NonNegative where
  arbitrary = nonNegative <$> chooseInt (0, 1000)
  shrink n =
    [nonNegative 0, nonNegative (getNonNegative n `div` 2), nonNegative (getNonNegative n `div` 2 + 1)]

prop_nonNegative n = getNonNegative n > 0

prop_nonZero n = (n :: Int) =/= 0

prop_factorial n = factorial n === product [1 .. getNonNegative n]

return []

main = do
  success <- $verboseCheckAll
  unless success exitFailure
