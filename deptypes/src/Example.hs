{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Data.Singletons (Sing, SingI, sing, withSingI)
import Data.Singletons.TH (genSingletons)

data DoorState = Open | Closed | Locked deriving (Eq)

genSingletons [''DoorState]

newtype Door (s :: DoorState) = UnsafeDoor String

sopen :: Sing Open
sopen = SOpen

lockAnyDoor :: forall s. SingI s => Door s -> Door Locked
lockAnyDoor = case sing @s of
  SOpen -> lock . close
  SClosed -> lock
  SLocked -> id

close :: Door 'Open -> Door 'Closed
close = _

lock :: Door 'Closed -> Door 'Locked
lock = _

promote :: DoorState -> (forall (s :: DoorState). SingI s => r) -> r
promote Open prog = prog @Open
promote Closed prog = prog @Closed
promote Locked prog = prog @Locked
