{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Functor.Dialects where

import Text.Read.Deriving (deriveRead1)
import Text.Show.Deriving (deriveShow1)

data Add a = a :+: a | a :-: a
  deriving (Read, Show, Functor, Foldable, Traversable)

data Mul a = a :*: a deriving (Read, Show, Functor, Foldable, Traversable)

data Div a = a :/: a deriving (Read, Show, Functor, Foldable, Traversable)

deriveRead1 ''Add
deriveRead1 ''Mul
deriveRead1 ''Div

deriveShow1 ''Add
deriveShow1 ''Mul
deriveShow1 ''Div
