{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Deptypes where

-- length :: String -> Int
-- length "" == 0
-- length "hello" == 5

-- id :: forall a. a -> a
-- Int :: *
-- data [a] = [] | a : [a] :: * -> *
-- value -> type???

data DoorState = Opened | Closed | Locked

data Door (s :: DoorState) where
  UnsafeDoor :: String -> Door s

-- newtype OpenedDoor = MkOpened String
-- newtype ClosedDoor = MkClosed String
-- newtype LockedDoor = MkLocked String

data Nat = Z | S Nat

data Array a (n :: Nat) where
  Nil :: Array a Z
  Cons :: a -> Array a n -> Array a (S n)

closedDoor :: String -> Door Closed
closedDoor = UnsafeDoor

lock :: Door Closed -> Door Locked
lock (UnsafeDoor m) = UnsafeDoor m

data Type = Type :->: Type | Boolean

data Value (a :: Type) where
  VBool :: Bool -> Value Boolean
  VFun :: (Value a -> Value b) -> Value (a :->: b)

data Term (a :: Type) where
  Const :: Bool -> Term Boolean
  App :: Term (a :->: b) -> Term a -> Term b
  Abs :: (Term a -> Term b) -> Term (a :->: b)

lift :: Value a -> Term a
lift (VBool b) = Const b
lift (VFun f) = Abs (lift . f . eval)

eval :: Term a -> Value a
eval (App f x) = case eval f of
  VFun f -> f (eval x)
eval (Abs f) = VFun (eval . f . lift)
