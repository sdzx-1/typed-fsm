{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypedFsm.Driver.Common where

import Data.IFunctor (At (..))
import Data.Singletons (Sing, SingI (..), SingKind (..))
import TypedFsm.Core (Operate (..), StateTransMsg (Msg))
import Unsafe.Coerce (unsafeCoerce)

data SomeOperate ts m a
  = forall (i :: ts) (o :: ts).
    (SingI i) =>
    SomeOperate (Operate m (At a o) i)

getSomeOperateSingeton :: (SingKind ts) => SomeOperate ts m a -> Sing ts
getSomeOperateSingeton (SomeOperate (_ :: Operate m ia i)) =
  unsafeCoerce $ sing @i

getSomeOperateSt :: (SingKind ts) => SomeOperate ts m a -> Demote ts
getSomeOperateSt (SomeOperate (_ :: Operate m ia i)) = fromSing $ sing @i

data SomeMsg ps from
  = forall (to :: ps).
    (SingI to) =>
    SomeMsg (Msg ps from to)

data AnyMsg ps
  = forall (from :: ps) (to :: ps).
    (SingI from, SingI to) =>
    AnyMsg (Msg ps from to)

{- | Reuslt of run FSM

* Finish, return val a
* A wrapper for SomeOperate that returns the remaining computation when there is not enough input
* Error happened
-}
data Result ps e m a
  = Finish a
  | Cont (SomeOperate ps m a)
  | ErrorInfo e