module Data.Group where

-- (0.5 балла) Закончите объявление класса `Group`, добавив в него реализации по
-- умолчанию для взятия обратного и деления в группе.

infixl 6 </>

class Monoid a => Group a where
  {-# MINIMAL inverse | (</>) #-}

  inverse :: a -> a
  -- ^ Group inverse.
  inverse = error "TODO: `inverse`"

  (</>) :: a -> a -> a
  -- ^ Group division.
  x </> y = error "TODO: `</>`"
