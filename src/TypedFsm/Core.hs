{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module TypedFsm.Core where

import Data.IFunctor (
  At (..),
  IFunctor (..),
  IMonad (..),
  returnAt,
  type (~>),
 )

import Data.Kind (Type)
import Data.Singletons (Sing, SingI (..))

-- | The state-transition type class
class StateTransMsg ps where
  data Msg ps (st :: ps) (st' :: ps)

{- | Core AST

Essentially all we do is build this AST and then interpret it.

`Operate m ia st` is an instance of `IMonad`, and it contains an `m` internally


Typed-fsm only contains two core functions: `getInput`, `liftm`.
We use these two functions to build Operate.

The overall behavior is as follows: constantly reading messages from the outside and converting them into internal monads action.
-}
data Operate :: (Type -> Type) -> (ps -> Type) -> ps -> Type where
  IReturn :: ia (mode :: ps) -> Operate m ia mode
  LiftM
    :: Sing mode'
    -> m (Operate m ia mode')
    -> Operate m ia mode
  In
    :: forall ps m (from :: ps) ia
     . (Msg ps from ~> Operate m ia)
    -> Operate m ia from

instance (Functor m) => IFunctor (Operate m) where
  imap f = \case
    IReturn ia -> IReturn (f ia)
    LiftM s f' -> LiftM s (fmap (imap f) f')
    In cont -> In (imap f . cont)
instance (Functor m) => IMonad (Operate m) where
  ireturn = IReturn
  ibind f = \case
    IReturn ia -> (f ia)
    LiftM s m -> LiftM s (fmap (ibind f) m)
    In cont -> In (ibind f . cont)

-- | get messages from outside
getInput :: forall ps m (from :: ps). (Functor m) => Operate m (Msg ps from) from
getInput = In ireturn

-- | lifts the internal `m a` to `Operate m (At a i) i'
liftm :: forall ps m (mode :: ps) a. (Functor m, SingI mode) => m a -> Operate m (At a mode) mode
liftm m = LiftM sing (returnAt <$> m)

liftConstr :: (SingI st', Applicative m) => ia st' -> Operate m ia st
liftConstr a = LiftM sing (pure $ ireturn a)