{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Free (Free)
import Data.Functor.Foldable (Base, Corecursive (ana), Recursive (project), cata, histo, refold)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (partition)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data List a = Nil | Cons a (List a) deriving (Foldable)

data BinTree a = Tip | Branch (BinTree a) a (BinTree a) deriving (Foldable)

-- treeFold :: (b -> a -> b -> b) -> b -> BinTree a -> b
-- treeFold _ init Tip = init
-- treeFold iter init (Branch l x r) = iter (treeFold iter init l) x (treeFold iter init r)

-- (b -> a -> b -> b, b)
-- ((b, a, b) -> b, () -> b)
-- ((b, a, b) | ()) -> b

-- data BinTreeF a b = TipF | BranchF b a b deriving (Functor)

-- treeFold' :: (BinTreeF a b -> b) -> BinTree a -> b
-- treeFold' alg = treeFold (\l x r -> alg $ BranchF l x r) (alg TipF)

-- class Recursive t where
--   type Base t :: * -> *
--   unwrap :: t -> Base t t

-- instance Recursive (BinTree a) where
--   type Base (BinTree a) = BinTreeF a

--   unwrap Tip = TipF
--   unwrap (Branch l x r) = BranchF l x r

-- fold :: (Recursive t, Functor (Base t)) => (Base t b -> b) -> t -> b
-- fold alg = alg . fmap (fold alg) . unwrap

-- depth :: BinTree a -> Int
-- depth = fold $ \case
--   TipF -> 0
--   BranchF l _ r -> 1 + l `max` r

-- class Corecursive t where
--   type Cobase t :: * -> *
--   wrap :: Cobase t t -> t

-- unfold :: (Corecursive t, Functor (Cobase t)) => (b -> Cobase t b) -> b -> t
-- unfold coalg = wrap . fmap (unfold coalg) . coalg

makeBaseFunctor ''BinTree

depth :: BinTree a -> Int
depth = cata $ \case
  TipF -> 0
  BranchF l _ r -> 1 + l `max` r

para :: Recursive t => (Base t (t, a) -> a) -> t -> a
para f = f . fmap (\t -> (t, para f t)) . project

-- data Cofree f a = a :< f (Cofree f a)

-- histo :: Recursive t => (Base t (Cofree (Base t) a) -> a) -> t -> a
-- histo f = error "histo is not defined"

-- * examples

-- * include

-- * .github

--   * workflows
--      ...

-- * src

--   * Data
--     * Functor
--       ...
--

-- * .github/workflows

-- * examples

-- * include

-- * src/Data/Functor

data Tree a = File a | Directory a [Tree a]

makeBaseFunctor ''Tree

pprint :: Tree String -> String
pprint tree = histo cases tree 0
  where
    cases :: Base (Tree String) (Cofree (Base (Tree String)) (Int -> String)) -> Int -> String
    cases (FileF name) indent = replicate indent ' ' <> name
    cases (DirectoryF name [_ :< DirectoryF name' sub]) indent =
      cases (DirectoryF (name <> "/" <> name') sub) indent
    cases (DirectoryF name xs) indent =
      replicate indent ' ' <> name <> "\n" <> unlines [x (indent + 2) | (x :< _) <- xs]

genTree :: Int -> BinTree ()
genTree = ana $ \case
  1 -> TipF
  n -> BranchF (n `div` 2) () (n - (n `div` 2))

-- genTree 5 = Branch (genTree 2) () (genTree 3) = Branch (Branch (genTree 1) () (genTree 1)) () (Branch (genTree 1) () (genTree 2))

-- apo :: Corecursive t => (a -> Base t (Either t a)) -> a -> t

-- futu :: Corecursive t => (a -> Base t (Free (Base t) a)) -> a -> t

-- refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b

quicksort :: Ord a => [a] -> [a]
quicksort = refold merge split
  where
    merge :: BinTreeF a [a] -> [a]
    merge TipF = []
    merge (BranchF l x r) = l ++ x : r

    split :: Ord a => [a] -> BinTreeF a [a]
    split [] = TipF
    split (x : xs) = BranchF l x r
      where
        (l, r) = partition (< x) xs

