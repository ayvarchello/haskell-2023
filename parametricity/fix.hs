mmul :: Monoid m => (m, m) -> m
mmul = uncurry (<>)

unit :: Monoid m => () -> m
unit = const mempty

-- ((m, m) -> m, () -> m)
-- Either (m, m) () -> m

data MonoidF m = MAppend m m | MEmpty

monoidAlgebra :: Monoid m => MonoidF m -> m
monoidAlgebra (MAppend x y) = x <> y
monoidAlgebra MEmpty = mempty

type FAlgebra f a = f a -> a

data SemiringF r = RZero | ROne | RPlus r r | RMul r r deriving (Functor)

newtype SemiringExpr = SExpr (SemiringF SemiringExpr)

zero :: SemiringExpr
zero = SExpr RZero

one :: SemiringExpr
one = SExpr ROne

plus :: SemiringExpr -> SemiringExpr -> SemiringExpr
plus e f = SExpr (RPlus e f)

mul :: SemiringExpr -> SemiringExpr -> SemiringExpr
mul e f = SExpr (RMul e f)

expr :: SemiringExpr
expr = (one `plus` one) `mul` (one `plus` one)

evalS :: FAlgebra SemiringF a -> SemiringExpr -> a
evalS f (SExpr e) = f $ fmap (evalS f) e

newtype Fix f = MkFix {unFix :: f (Fix f)}
-- f (f (f (...)))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

data Nat = Zero | Succ Nat

data NatF a = ZeroF | SuccF a

-- Nat ~ Fix NatF

ana :: Functor f => (a -> f a) -> a -> Fix f
ana g = MkFix . fmap (ana g) . g

-- data Stream a = a :| Stream a

data StreamF a f = a :| f deriving (Functor)

type Stream a = Fix (StreamF a)

natFrom :: Integer -> Stream Integer
natFrom = ana (\n -> n :| (n + 1))

cons :: a -> Stream a -> Stream a
cons x y = MkFix (x :| y)

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p = cata (\(x :| rest) -> if p x then x `cons` rest else rest)

primes :: Stream Integer
primes =
  ana
    (\(MkFix (p :| rest)) -> p :| filterS (\x -> x `mod` p == 0) rest)
    (natFrom 2)

-- data [a] = [] | a : [a]

data ListF a x = NilF | ConsF a x deriving (Functor)

type List a = Fix (ListF a)

cataList :: (ListF a b -> b) -> List a -> b
-- ((1 + a b) -> b) -> List a -> b
-- ((1 -> b) * (a b -> b)) -> List a -> b
-- (b * (a -> b -> b)) -> List a -> b
-- b -> (a -> b -> b) -> List a -> b
-- (a -> b -> b) -> b -> List a -> b
-- (a -> b -> b) -> b -> [a] -> b
cataList = cata
