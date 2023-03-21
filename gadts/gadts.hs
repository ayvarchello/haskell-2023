{-# LANGUAGE GADTs #-}

data Expr = Const Int | Expr :+: Expr | Expr :*: Expr

eval :: Expr -> Int
eval (Const x) = x
eval (x :+: y) = eval x + eval y
eval (x :*: y) = eval x * eval y

data Expr'
  = Const' Int
  | Expr' :++: Expr'
  | Expr' :**: Expr'
  | Embed Bool
  | If Expr' Expr' Expr'

eval' :: Expr' -> Either Bool Int
eval' (Const' x) = Right x
eval' (Embed b) = Left b
eval' (If (Embed True) left right) = eval' left
eval' (If (Embed False) left right) = eval' right
eval' (left :++: right) = _
eval' (left :**: right) = _
eval' (If cond left right) = _

data Value = IntValue Int | BoolValue Bool

data EvalError

eval'' :: Expr' -> Either EvalError Value
eval'' = _

-- data Tree a = Leaf | Branch a [Tree a]

data WTExpr a where
  WTConst :: Int -> WTExpr Int
  WTBool :: Bool -> WTExpr Bool
  (:+++:) :: WTExpr Int -> WTExpr Int -> WTExpr Int
  WTIf :: WTExpr Bool -> WTExpr a -> WTExpr a -> WTExpr a

wtEval :: WTExpr a -> a
wtEval (WTConst x) = x
wtEval (WTBool b) = b
wtEval (x :+++: y) = wtEval x + wtEval y
wtEval (WTIf cond left right) =
  if wtEval cond then wtEval left else wtEval right

data ExistentialGADT where
  ExShow :: Show a => a -> ExistentialGADT

data Term a where
  Emb :: a -> Term a
  Lam :: (Term a -> Term b) -> Term (a -> b)
  App :: Term (a -> b) -> Term a -> Term b

evalTerm :: Term a -> a
evalTerm (App f x) = evalTerm f (evalTerm x)
evalTerm (Lam f) = evalTerm . f . Emb

-- (\y -> (\x -> x y) (\y -> y))
-- data Term = Var String | App Term Term | Lam String Term
-- data Type = TInt | TBool | ... | TFun Type Type
--
-- data Term' = App' Term' Term' | Lam' ???
-- Lam' (\y -> App' (Lam' (\x -> App' x y)) (Lam' (\y -> y)))





