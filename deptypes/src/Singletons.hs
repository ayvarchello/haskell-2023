{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Singletons where
import GHC.TypeNats (KnownNat)

data DoorState = Open | Closed | Locked

data DoorSing (s :: DoorState) where
  SOpen :: DoorSing Open
  SClosed :: DoorSing Closed
  SLocked :: DoorSing Locked

class SingDSI s where
  singDS :: DoorSing s

instance SingDSI Open where
  singDS = SOpen

instance SingDSI Closed where
  singDS = SClosed

instance SingDSI Locked where
  singDS = SLocked

data Door (s :: DoorState) where
  UnsafeDoor :: String -> Door s

unsafeDoor :: DoorSing s -> String -> Door s
unsafeDoor = \case
  SOpen -> UnsafeDoor
  SClosed -> UnsafeDoor
  SLocked -> UnsafeDoor

close :: Door Open -> Door Closed
close = _

lock :: Door Closed -> Door Locked
lock = _

lockAnyExplicit :: DoorSing s -> Door s -> Door Locked
lockAnyExplicit SOpen = lock . close
lockAnyExplicit SClosed = lock
lockAnyExplicit SLocked = id

lockAny :: SingDSI s => Door s -> Door Locked
lockAny = lockAnyExplicit singDS

data Nat = Z | S Nat

data NatSing (n :: Nat) where
  ZSing :: NatSing Z
  SSing :: NatSing n -> NatSing (S n)

class SingNat n where
  singNat :: NatSing n

instance SingNat Z where
  singNat = ZSing

instance SingNat n => SingNat (S n) where
  singNat = SSing singNat

nat :: NatSing n -> Nat
nat ZSing = Z
nat (SSing n) = S (nat n)

data Array a (n :: Nat) where
  Nil :: Array a Z
  Cons :: a -> Array a n -> Array a (S n)

len :: SingNat n => Array a n -> Nat
len _ = nat singNat

KnownNat




