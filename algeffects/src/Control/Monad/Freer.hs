{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Freer where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (ap, (>=>))
import System.IO (Handle)
import Data.Functor.Sum (Sum (..))

-- MonadReader, MonadState

-- StateT, ReaderT, WriterT
-- MonadState, MonadReader, MonadWrite
--
-- MonadState StateT
-- MonadReader ReaderT
-- MonadWrite WriterT
--
-- MonadReader m => MonadReader (StateT s m)
-- MonadReader m => MonadReader (WriterT c m)

-- MonadScheme, MonadBank

-- scheme :: MonadScheme a m => a -> a -> m (a, a)

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free fa) = Free (fmap (fmap g) fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure a >>= g = g a
  Free fa >>= g = Free (fmap (>>= g) fa)

-- data Add a = a :+: a | a :-: a deriving (Functor)
-- data Mul a = a :*: a deriving (Functor)
-- data Sum f g a = InL (f a) | InR (g a) deriving (Functor)
-- type AddAndMul = Sum Add Mul
-- Alexander Granin

data Freer f a where
  Return :: a -> Freer f a
  Bind :: f i -> (i -> Freer f a) -> Freer f a

-- retry#
-- delimcc#
-- callcc#

data Arr f a b where
  Id :: Arr f a (Freer f a)
  (:*:) :: (b -> Freer f c) -> Arr f a (Freer f b) -> Arr f a (Freer f c)

-- freer, more extensible monads
-- by Oleg Kiselyov

instance Functor (Freer f) where
  fmap f x = x >>= return . f

instance Applicative (Freer f) where
  pure = Return
  (<*>) = ap

instance Monad (Freer f) where
  Return a >>= g = g a
  Bind fi k >>= g = Bind fi (k >=> g)

interpret :: Monad m => (forall a. f a -> m a) -> Freer f a -> m a
interpret ops (Return a) = return a
interpret ops (Bind fi k) = ops fi >>= interpret ops . k

class f :<: g where
  inj :: f a -> g a

instance f :<: f where
  inj = id

instance f :<: Sum f g where
  inj = InL

instance f :<: g => f :<: Sum h g where
  inj = InR . inj

data ReadOps o where
  ReadChar :: Handle -> ReadOps Char

-- ReadOps :: * -> *

readChar :: ReadOps :<: f => Handle -> Freer f Char
readChar h = Bind (inj $ ReadChar h) Return

data WriteOps o where
  WriteChar :: Char -> Handle -> WriteOps ()

-- WriteOps :: * -> *

-- class f :<: g where
--    inj :: f a -> g a

writeChar :: WriteOps :<: f => Char -> Handle -> Freer f ()
writeChar c h = Bind (inj $ WriteChar c h) Return

echo :: (ReadOps :<: f, WriteOps :<: f) => Handle -> Freer f ()
echo h = do
  c <- readChar h
  writeChar c h

type Echo a = Freer (Sum ReadOps WriteOps) a

interpretIO :: Echo a -> IO a
interpretIO = interpret $ \case
  InL (ReadChar h) -> _
  InR (WriteChar c h) -> _



