{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.NonNegative (NonNegative (getNonNegative), nonNegative) where

newtype NonNegative = NonNegative {getNonNegative :: Int}
  deriving newtype (Real, Integral)
  deriving (Show, Eq, Ord)

nonNegative :: Int -> NonNegative
nonNegative n
  | n >= 0 = NonNegative n
  | otherwise = error "n is negative"

instance Num NonNegative where
  (+) = _
  (*) = _
  abs = _
  signum = _
  fromInteger = _
  negate = _

instance Enum NonNegative where
  toEnum = nonNegative
  fromEnum = getNonNegative
