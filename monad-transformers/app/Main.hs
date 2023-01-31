{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (ap, join)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  x <- getLine
  putStrLn x

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- instance Monad Maybe
-- instance Monad ((->) e)

data Either' e a = Left' !e | Right' !a deriving (Functor)

instance Applicative (Either' e) where
  pure = Right'
  (<*>) = ap

instance Monad (Either' e) where
  Left' e' >>= f = Left' e'
  Right' a >>= f = f a

data AppError = DivByZero

safeDiv :: (Fractional a, Eq a) => a -> a -> Either AppError a
safeDiv x y
  | y == 0 = Left DivByZero
  | otherwise = Right (x / y)

data Term
  = Var !String
  | Const !Rational
  | Div !Term !Term
  | Abs !String !Term
  | App !Term !Term

data Value
  = RationalValue !Rational
  | Closure !Env !String !Term

type Env = HashMap String Value

eval :: Term -> ((->) Env) (Either AppError Value)
eval (Var s) e = let Just v = M.lookup s e in Right v
eval (Const n) e = Right (RationalValue n)
eval (Div te te') e = do
  left <- eval te e
  right <- eval te' e
  case (left, right) of
    (RationalValue left, RationalValue right) ->
      RationalValue <$> safeDiv left right
    _ -> error "not numbers"
eval (Abs s te) e = Right $ Closure e s te
eval (App te te') e = error "TODO"

newtype Compose (m :: * -> *) (n :: * -> *) (a :: *) = Compose
  { runCompose :: m (n a)
  }

type PartialCompose m n = Compose m n :: * -> *

type AppCompose a = Compose ((->) Env) (Either AppError) a

instance (Functor m, Functor n) => Functor (Compose m n) where
  fmap f = Compose . fmap @m (fmap @n f) . runCompose

-- newtype Identity a = Identity { runIdentity :: a }

instance (Applicative m, Applicative n) => Applicative (Compose m n) where
  pure = Compose . pure . pure

  -- (<*>) :: g (a -> b) -> (g a -> g b)
  (<*>) (Compose f) = Compose . distribute f . runCompose
    where
      distribute :: m (n (a -> b)) -> (m (n a) -> m (n b))
      distribute = (<*>) . fmap (<*>)

instance (Monad m, Monad n) => Monad (Compose m n) where
  (>>=) = viaJoin joinCompose

joinCompose ::
  (Monad m, Monad n) => Compose m n (Compose m n a) -> Compose m n a
joinCompose = Compose . _ . runCompose . fmap runCompose

join' :: Monad m => m (m a) -> m a
join' x = x >>= id

viaJoin ::
  Functor f =>
  (forall c. f (f c) -> f c) ->
  f a ->
  (a -> f b) ->
  f b
viaJoin join x f = join $ fmap f x

-- newtype Reader e a = Reader {runReader :: e -> a}

-- newtype ReaderT e n a = ReaderT {runReaderT :: e -> n a}

-- instance Functor m => Functor (ReaderT e m)

-- instance Applicative m => Applicative (ReaderT e m)

-- instance Monad m => Monad (ReaderT e m) where
--   ReaderT x >>= f = ReaderT $ \e -> x e >>= ($ e) . runReaderT . f

-- ask' :: Reader e e
-- ask' = Reader $ \e -> e

ask :: forall e m. Applicative m => ReaderT e m e
ask = ReaderT (\e -> pure @m e)

lift :: m a -> ReaderT e m a
lift x = ReaderT $ const x

eval' :: Term -> ReaderT Env (Either AppError) Value
eval' (Var s) = do
  e <- ask
  let Just v = M.lookup s e in pure v
eval' (Const n) = pure (RationalValue n)
eval' (Div te te') = do
  left <- eval' te
  right <- eval' te'
  case (left, right) of
    (RationalValue left, RationalValue right) ->
      RationalValue <$> lift (safeDiv left right)
    _ -> error "not numbers"
eval' (Abs s te) = do
  e <- ask
  pure $ Closure e s te
eval' (App te te') = error "TODO"




