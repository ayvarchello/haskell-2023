{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (ap, join)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Control.Monad.Trans.State (StateT (runStateT), evalStateT, get, put)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import qualified Control.Monad.State.Strict as S

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

--instance (Monad m, Monad n) => Monad (Compose m n) where
--  (>>=) = viaJoin joinCompose

--joinCompose ::
--  (Monad m, Monad n) => Compose m n (Compose m n a) -> Compose m n a
--joinCompose = Compose . _ . runCompose . fmap runCompose

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

-- class Monad m => MonadReader e m where
--   askGeneric :: m e

-- instance MonadReader e ((->) e) where
--   askGeneric = id

-- instance Monad m => MonadReader e (ReaderT e m) where
--   askGeneric = ask

-- instance MonadReader e m => MonadReader e (InputT m) where
--   askGeneric = lift askGeneric

-- lift :: m a -> ReaderT e m a
-- lift x = ReaderT $ const x

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

main :: IO ()
main = evalStateT (runInputT defaultSettings app) []

app :: InputT (StateT [String] IO) ()
app = do
  line <- getInputLine "> "
  case line of
    Just line -> do
      state <- lift get
      for_ state outputStrLn
      lift (put (line : state))
      app
    Nothing -> outputStrLn "Bye!"

eval'' ::
  (MonadReader Env m, MonadError AppError m) => Term -> m Value
eval'' (Var s) = do
  e <- ask
  let Just v = M.lookup s e in pure v
eval'' (Const n) = pure (RationalValue n)
eval'' (Div te te') = do
  left <- eval'' te
  right <- eval'' te'
  case (left, right) of
    (RationalValue left, RationalValue right) ->
      RationalValue <$> liftEither (safeDiv left right)
    _ -> error "not numbers"
eval'' (Abs s te) = do
  e <- ask
  pure $ Closure e s te
eval'' (App te te') = error "TODO"

data Scheme a
  = SPin a
  | SAnd (Scheme a) (Scheme a)
  | SOr (Scheme a) (Scheme a)
  | SNot (Scheme a)

tmp = SAnd (SPin "x") (SPin "y")
result = SOr tmp (SNot tmp)

-- x => and => or

data Vertex a = VAnd !a !a | VOr !a !a | VNot !a

data Scheme' = MkScheme { inputs :: !Int, gates :: ![Vertex Int] }

appendAnd :: Int -> Int -> Scheme' -> Scheme'
appendAnd x y s = MkScheme { inputs = inputs s, gates = VAnd x y : gates s }

class Monad m => MonadGate a m where
  gate :: Vertex a -> m a

instance (S.MonadState Scheme' m, Monad m) => MonadGate Int m where
  gate v = do
    state <- S.get
    let vName = inputs state + length (gates state)
    S.put (MkScheme (inputs state) (v : gates state))
    pure vName

makeAnd :: MonadGate a m => a -> a -> m a
makeAnd x y = do
  dup <- gate (VAnd x x)
  dup' <- gate (VAnd y y)
  gate (VOr dup dup')

runGates :: (forall a m. MonadGate a m => [a] -> m ()) -> Int -> Scheme'
runGates instruction inputs =
  S.execState (instruction [0..(inputs - 1)]) (MkScheme inputs [])

instance MonadGate Bool Identity where
  gate (VAnd a a') = Identity (a && a')
  gate (VOr a a') = Identity (a || a')
  gate (VNot a) = Identity (not a)

andResult x y = runIdentity (makeAnd x y) :: Bool

prop_and x y = andResult x y == x && y
