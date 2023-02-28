{-# LANGUAGE RecordWildCards #-}

module Main (main) where

data OldState = MkOldState {position :: Int, datum :: [Int]}

moveLeft :: OldState -> OldState
moveLeft state = state {position = position state - 1}

moveRight :: OldState -> OldState
moveRight state = state {position = position state + 1}

increment :: OldState -> OldState
increment MkOldState {..} =
  MkOldState position $
    take position datum
      ++ [succ (datum !! position)]
      ++ drop (position + 1) datum

data ListZipper a = Top [a] | Middle [a] a [a]

-- List A = 1 + A x List A
-- L = 1 + A L
-- L (1 - A) = 1
-- L = 1 / (1 - A) = 1 + A + A^2 + A^3 + ...
-- dL/dA = -1/(1 - A)^2 * d(1 - A)/dA = 1/(1 - A)^2
-- dL/dA = 1 + 2A + 3A^2 + 4A^3 + ...
-- [^]
-- [^a] | [a^]
-- [^aa] | [a^a] | [aa^]
-- [^aaa] | [a^aa] | [aa^a] | [aaa^]
--
-- (a + b)' = a' + b'
-- (a * b)' = a' * b + a * b'
-- x' = 1
--
-- P = 1/(1 - A)^2
-- P = 1/(1 - A)/(1 - A) = L / (1 - A)
-- P (1 - A) = L
-- P = L + P A

-- L = 1 + A L
-- L' = (1 + A L)'
-- L' = A' L + A L'
-- L' = L + A L'
data Zipper a = PTop [a] | PMiddle a (Zipper a)

type ListLoc a = (Zipper a, [a])

-- L + A (L + A (L + ...))
-- L + A L + A A L + ...
type NewZipper a = ([a], [a])

moveLeftZipper :: ListZipper Int -> Maybe (ListZipper Int)
moveLeftZipper (Top _) = Nothing
moveLeftZipper (Middle (x : ys) z ws) = Just $ Middle ys x (z : ws)
moveLeftZipper (Middle [] z ws) = Just $ Top (z : ws)

moveRightZipper :: ListZipper Int -> Maybe (ListZipper Int)
moveRightZipper (Top []) = Nothing
moveRightZipper (Top (x : ys)) = Just $ Middle [] x ys
moveRightZipper (Middle xs y []) = Nothing
moveRightZipper (Middle xs y (z : ws)) = Just $ Middle (y : xs) z ws

incrementZipper :: ListZipper Int -> Maybe (ListZipper Int)
incrementZipper (Top _) = Nothing
incrementZipper (Middle xs y zs) = Just $ Middle xs (succ y) zs

data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- T = a + T T
-- T' = 1 + T' T + T T'

data TreeZipper a
  = TRoot
  | TLeft (TreeZipper a) (Tree a)
  | TRight (Tree a) (TreeZipper a)

type TreeLoc a = (TreeZipper a, Tree a)

up :: TreeLoc a -> TreeLoc a
up (TRoot, t) = (TRoot, t)
up (TLeft p r, l) = (p, Branch l r)
up (TRight l p, r) = (p, Branch l r)

main :: IO ()
main = putStrLn "Hello, Haskell!"
