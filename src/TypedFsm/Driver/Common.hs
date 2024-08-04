{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypedFsm.Driver.Common where

import Data.IFunctor (At (..))
import Data.Singletons (Sing, SingKind (..))
import TypedFsm.Core (Operate (..), StateTransMsg (Msg))
import Unsafe.Coerce (unsafeCoerce)

data SomeOperate ts m a
  = forall (i :: ts) (o :: ts).
    SomeOperate (Sing i) (Operate m (At a o) i)

getSomeOperateSing :: SomeOperate ts m a -> Sing (r :: ts)
getSomeOperateSing (SomeOperate si (_ :: Operate m ia i)) =
  unsafeCoerce si

getSomeOperateSt :: (SingKind ts) => SomeOperate ts m a -> Demote ts
getSomeOperateSt (SomeOperate si (_ :: Operate m ia i)) = fromSing $ si

data SomeMsg ps from
  = forall (to :: ps).
    SomeMsg (Sing to) (Msg ps from to)

data AnyMsg ps
  = forall (from :: ps) (to :: ps).
    AnyMsg (Sing from) (Sing to) (Msg ps from to)

{- | Reuslt of run FSM

* Finish, return val a
* A wrapper for SomeOperate that returns the remaining computation when there is not enough input
* Error happened
-}
data Result ps e m a
  = Finish a
  | Cont (SomeOperate ps m a)
  | ErrorInfo e