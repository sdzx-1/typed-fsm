{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module TypedFsm.Core where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import Data.SR

import Data.Kind (Type)

class StateTransMsg ps where
  data Msg ps (st :: ps) (st' :: ps)

data Operate :: (Type -> Type) -> (ps -> Type) -> ps -> Type where
  IReturn :: ia (mode :: ps) -> Operate m ia mode
  LiftM
    :: ( SingI mode
       , Reify mode
       , SingI mode'
       , Reify mode'
       )
    => m (Operate m ia mode')
    -> Operate m ia mode
  In
    :: forall ps m (from :: ps) ia
     . (Msg ps from ~> Operate m ia)
    -> Operate m ia from

instance (Functor m) => IFunctor (Operate m) where
  imap f = \case
    IReturn ia -> IReturn (f ia)
    LiftM f' -> LiftM (fmap (imap f) f')
    In cont -> In (imap f . cont)
instance (Functor m) => IMonad (Operate m) where
  ireturn = IReturn
  ibind f = \case
    IReturn ia -> (f ia)
    LiftM m -> LiftM (fmap (ibind f) m)
    In cont -> In (ibind f . cont)

getInput :: forall ps m (from :: ps). (Functor m) => Operate m (Msg ps from) from
getInput = In ireturn

liftm :: forall ps m (mode :: ps) a. (Functor m, SingI mode, Reify mode) => m a -> Operate m (At a mode) mode
liftm m = LiftM (returnAt <$> m)
