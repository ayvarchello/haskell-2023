{-# LANGUAGE OverlappingInstances #-}

import Control.Monad (ap, join)
import Data.Foldable (for_)

data MonoidF a = MEmpty | MAppend a a deriving (Functor)

data MonoidExpr x = MVar x | MOp (MonoidF (MonoidExpr x))

var :: x -> MonoidExpr x
var = MVar

empty :: MonoidExpr x
empty = MOp MEmpty

append :: MonoidExpr x -> MonoidExpr x -> MonoidExpr x
append l r = MOp (MAppend l r)

expr :: MonoidExpr Char
expr = var 'x' `append` var 'y' `append` var 'z'

evalM :: (x -> y) -> (MonoidF y -> y) -> MonoidExpr x -> y
evalM v f (MVar x) = v x
evalM v f (MOp g) = f $ fmap (evalM v f) g

data Free f x = Pure x | Free (f (Free f x)) deriving (Functor)

embed :: Functor f => f a -> Free f a
embed x = Free (fmap Pure x)

eval :: Functor f => (x -> a) -> (f a -> a) -> Free f x -> a
eval v f (Pure x) = v x
eval v f (Free g) = f $ fmap (eval v f) g

free :: f (Free f x) -> Free f x
free = Free

substitute :: Functor f => (x -> Free f y) -> Free f x -> Free f y
substitute g = eval g free

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (>>=) = flip substitute

evalMonad :: Monad f => Free f a -> f a
evalMonad = eval pure join

mapFunctor :: Functor f => (forall b. f b -> g b) -> Free f a -> Free g a
mapFunctor n = eval Pure (Free . n)

-- id :: a -> a
-- f :: [a] -> [a]

data Lang a deriving (Functor)

data Lang2 a deriving (Functor)

instruction :: Free Lang a
instruction = do
  x <- undefined
  return x

interpreter :: Lang a -> Lang2 a
interpreter = undefined

instruction2 :: Free Lang2 (a, b)
instruction2 = do
  y <- mapFunctor interpreter instruction
  z <- undefined
  return (y, z)

interpretIO :: Lang2 a -> IO a
interpretIO = undefined

-- main = evalMonad (mapFunctor interpretIO instruction2)

data (f :+: g) a = InL (f a) | InR (g a) deriving (Functor)

-- f :+: g :+: h = f :+: (g :+: h)

hybrid :: Free (Lang :+: Lang2) (a, (b, c))
hybrid = do
  x <- mapFunctor InL instruction
  y <- mapFunctor InR instruction2
  return (x, y)

data Telescope a = ReadChar (Char -> a) | WriteChar Char a deriving (Functor)

data FileSystem a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a
  deriving (Functor)

cat :: FilePath -> Free (Telescope :+: FileSystem) ()
cat path = do
  file <- embed $ InR (ReadFile path id)
  for_ file $ \c -> embed $ InL (WriteChar c ())

runIO :: (Telescope :+: FileSystem) a -> IO a
runIO = undefined

main :: IO ()
main = evalMonad $ mapFunctor runIO (cat "input.txt")

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance (Functor f, Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
  inj = InR . inj

mapFunctor' :: (f :<: g) => Free f a -> Free g a
mapFunctor' = eval Pure (Free . inj)

readFile' :: (FileSystem :<: f) => FilePath -> Free f String
readFile' path = embed . inj $ ReadFile path id

writeChar :: (Telescope :<: f) => Char -> Free f ()
writeChar c = embed . inj $ WriteChar c ()

cat' :: (FileSystem :<: f, Telescope :<: f) => FilePath -> Free f ()
cat' path = do
  file <- readFile' path
  for_ file writeChar

-- FileSystem :+: FileSystem

sanityCheck :: Free (FileSystem :+: Telescope) ()
sanityCheck = cat' "input.txt"
