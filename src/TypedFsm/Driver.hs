{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module TypedFsm.Driver where

import Control.Monad.State as S
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as D
import Data.GADT.Compare (GCompare)
import Data.IFunctor
import Data.SR
import TypedFsm.Core

newtype GenMsg ps state event from
  = GenMsg (state -> event -> Maybe (SomeMsg ps from))

type State2GenMsg ps state event = DMap (Sing @ps) (GenMsg ps state event)

data SomeMsg ps from
  = forall (to :: ps).
    (SingI to, Reify to) =>
    SomeMsg (Msg ps from to)

data SomeOperate ts m a
  = forall (i :: ts) (o :: ts).
    (SingI i, Reify i) =>
    SomeOperate (Operate m (At a o) i)

reifySomeOperateInput :: SomeOperate ts m a -> ts
reifySomeOperateInput (SomeOperate (_ :: Operate m ia i)) =
  reify @_ @i

type OpResult ps m a = (Either (SomeOperate ps m a) a)

type Op ps state o i = Operate (StateT state IO) (At () (o :: ps)) (i :: ps)

type SomeOp ps state = SomeOperate ps (StateT state IO) ()

runOp
  :: forall ps event state a (input :: ps) (output :: ps)
   . ( SingI input
     , Reify input
     , GCompare (Sing @ps)
     )
  => State2GenMsg ps state event
  -> [event]
  -> Operate (StateT state IO) (At a output) input
  -> (StateT state IO) (OpResult ps (StateT state IO) a)
runOp dmp evns = \case
  IReturn (At a) -> pure (Right a)
  LiftM m -> m Prelude.>>= runOp dmp evns
  In f -> do
    case D.lookup (sing @input) dmp of
      Nothing -> error "np"
      Just (GenMsg genMsg) -> loop evns
       where
        loop [] = pure $ Left $ SomeOperate (In f)
        loop (et : evns') = do
          state' <- get
          case genMsg state' et of
            Nothing -> loop evns'
            Just (SomeMsg msg) -> runOp dmp evns' (f msg)
