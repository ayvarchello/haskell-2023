{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BaseLaws where

import Control.Category ((.))
import Control.Monad (Monad, return, (>>=))
import Data.Bool (Bool)
import Data.Eq (Eq, (==))
import Data.Int (Int)
import GHC.Err (error)
import GHC.IOArray (IOArray)
import System.IO (IO)
import Data.Proxy (Proxy (Proxy))

infixr 5 <>

class Semigroup a where
  (<>) :: a -> a -> a

-- (Associativity) for all x, y, z
-- x <> (y <> z) = (x <> y) <> z

-- "Lawful instance"
-- "Unlawful instance"

testAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
testAssoc x y z = x <> (y <> z) == (x <> y) <> z

newtype Endo a = Endo {getEndo :: a -> a}

instance Semigroup (Endo a) where
  Endo g <> Endo f = Endo (g . f)
  -- h . (g . f) = (h . g) . f

endoAssoc :: Eq a => Endo a -> Endo a -> Endo a -> a -> Bool
endoAssoc x y z w = getEndo (x <> (y <> z)) w == getEndo ((x <> y) <> z) w

sqr :: Semigroup a => a -> a
sqr x = x <> x

cube :: Semigroup a => a -> a
cube x = x <> x <> x

class Semigroup a => Monoid a where
  mempty :: a

-- (Left identity) for all x,
-- mempty <> x = x

-- (Right identity) for all x,
-- x <> mempty = x

data HashTable a = HTable {table :: IOArray Int [a], hashingFunction :: a -> Int}

foldTable :: Monoid a => HashTable a -> IO a
foldTable = error "foldTable not implemented"

data BinaryTree = Leaf | Node BinaryTree BinaryTree

instance Semigroup BinaryTree where
  (<>) = Node

-- Leaf <> (Leaf <> Leaf) = Node Leaf (Node Leaf Leaf)
-- (Leaf <> Leaf) <> Leaf = Node (Node Leaf Leaf) Leaf

instance Monoid BinaryTree where
  mempty = Leaf

-- Leaf <> x = Node Leaf x /= x

getFolded :: HashTable BinaryTree -> IO BinaryTree
getFolded = foldTable

data Add a = a :+: a

instance Functor Add where
  f <$> (x :+: y) = f x :+: f y

data Free f a = Pure a | Free (f (Free f a))

type AddLanguage a = Free Add a

-- x :+: (y :+: z)
-- x :+: ((y :+: z) :+: w)

instance Semigroup (AddLanguage a) where
  x <> y = Free (x :+: y)

class Functor (f :: * -> *) where
  (<$>) :: forall a b. (a -> b) -> f a -> f b

-- id <$> x = x
-- (f . g) <$> x = f <$> (g <$> x)

iter :: Functor f => (f a -> a) -> Free f a -> a
iter alg (Pure x) = x
iter alg (Free fx) = alg (iter alg <$> fx)

semiIter :: Semigroup a => AddLanguage a -> a
semiIter = iter (\(x :+: y) -> x <> y)

class Monad m => MonadBank a b m | m -> a, m -> b where
  newAccount :: m a
  balance :: a -> m b
  deposit :: a -> b -> m ()
  withdraw :: a -> b -> m ()
  deleteAccount :: a -> m ()

-- dep b = return ()
--
-- where
--
-- dep b = do
--  x <- newAccount
--  deposit x b

bankLaw :: (Eq (m ()), MonadBank a b m) => Proxy m -> Proxy a -> b -> Bool
bankLaw (Proxy :: Proxy m) (Proxy :: Proxy a) b =
  (newAccount >>= (\x -> deposit @a @_ @m x b)) == return ()

-- x, y :: BankState -> (BankState, ())
-- x == y
