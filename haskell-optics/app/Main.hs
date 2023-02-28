{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Applicative (Applicative(liftA2))

data Point = Point2 {_x :: Float, _y :: Float}

makeLenses ''Point

newtype Color = RGB Int

data Sprite = Sprite {_pos :: Point, _color :: Color}

makeLenses ''Sprite

newtype Name = Name {_string :: String}

makeLenses ''Name

data Player = Player {_sprite :: Sprite, _name :: Name}

makeLenses ''Player

getColor :: Player -> Color
getColor p = p ^. sprite . color

setY :: Float -> Player -> Player
setY = set (sprite . pos . y)

nameLength :: Player -> Int
nameLength p = p ^. name . string . to length

updatePositions :: (Point -> Point) -> [Player] -> [Player]
updatePositions upd = map (\p -> set (sprite . pos) (p ^. sprite . pos . to upd) p)

--------- LENS DERIVATION -------

composition = (.) . (.) . (.)

fmaps ::
  (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmaps = fmap . fmap . fmap

compositionIsFmaps f g x y z = composition f g x y z == fmaps f g x y z

folds ::
  (Foldable f, Foldable g, Foldable h, Monoid m) => (a -> m) -> f (g (h a)) -> m
folds = foldMap . foldMap . foldMap

traverses ::
  (Traversable t, Traversable t', Applicative f) =>
  (a -> f b) ->
  t (t' a) ->
  f (t (t' b))
traverses = traverse . traverse

fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f = runIdentity . traverse (pure . f)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)

type MySetter s t a b = (a -> Identity b) -> s -> Identity t

over :: MySetter s t a b -> (a -> b) -> s -> t
over t f = runIdentity . t (Identity . f)

-- over t id = id
-- over t (g . f) = over t g . over t f

mysets :: ((a -> b) -> s -> t) -> MySetter s t a b
mysets t f = Identity . t (runIdentity . f)

mapped :: Functor f => MySetter (f a) (f b) a b
mapped = mysets fmap

type MyGetting r s a = (a -> Const r a) -> s -> Const r s

foldMapOf :: MyGetting r s a -> (a -> r) -> s -> r
foldMapOf t f = getConst . t (Const . f)

myfolds :: ((a -> r) -> s -> r) -> MyGetting r s a
myfolds t f = Const . t (getConst . f)

type MyFold s t a b = forall m. Monoid m => (a -> Const m b) -> s -> Const m t

folded :: Foldable f => MyFold (f a) (f a) a a
folded = myfolds foldMap

type MyTraversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

mytraverse :: Traversable t => MyTraversal (t a) (t b) a b
mytraverse = traverse

myboth :: MyTraversal (a, a) (b, b) a b
myboth f (x, y) = liftA2 (,) (f x) (f y)

traverseLeft :: MyTraversal (Either a c) (Either b c) a b
traverseLeft f (Left x) = fmap Left (f x)
traverseLeft _ (Right y) = pure (Right y)

what :: MyTraversal (Either a c, Either a c) (Either b c, Either b c) a b
what = myboth . traverseLeft

cps :: (a -> c) -> (c -> r) -> (a -> r)
cps f g = g . f

uncps :: (forall r. (c -> r) -> (a -> r)) -> (a -> c)
uncps f = f id

type MyGetter s a = forall r. (a -> Const r a) -> s -> Const r s

myget :: s -> MyGetting a s a -> a
myget s l = getConst (l Const s)

type MyLens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

main :: IO ()
main = putStrLn "Hello, Haskell!"
