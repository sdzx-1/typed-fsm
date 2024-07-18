{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{- | Running FSM

The core function is `runOp`, and the other functions are to make it work properly.
-}
module TypedFsm.Driver where

import Control.Monad.State as S (MonadState (get), StateT)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as D
import Data.GADT.Compare (GCompare, GOrdering (..))
import Data.IFunctor (At (..))
import Data.Ord.Singletons (SOrd (sCompare), SOrdering (..))
import Data.Singletons (Sing, SingI (..), SingKind (..))
import TypedFsm.Core (Operate (..), StateTransMsg (Msg))
import Unsafe.Coerce (unsafeCoerce)

data SomeOperate ts m a
  = forall (i :: ts) (o :: ts).
    (SingI i) =>
    SomeOperate (Operate m (At a o) i)

getSomeOperateSt :: (SingKind ts) => SomeOperate ts m a -> Demote ts
getSomeOperateSt (SomeOperate (_ :: Operate m (At a o) i)) = fromSing $ sing @i

{- | Reuslt of runOp

* Finish, return val a
* A wrapper for SomeOperate that returns the remaining computation when there is not enough input
* There is no corresponding GenMsg function defined for some FSM states
-}
data Result ps m a
  = Finish a
  | Cont (SomeOperate ps m a)
  | forall t. NotMatchGenMsg (Sing (t :: ps))

{- | `Op` adds new assumptions based on `Operate`: assume that the internal monad contains at least a state monad.

@
type Op ps state m a o i = Operate (StateT state m) (At a (o :: ps)) (i :: ps)
@

`Op` contains two states, `ps` and `state`.

`ps` represents the state of the state machine
`state` represents the internal state.

The external event needs to be converted to Msg.

It is essentially a function `event -> Msg`, but this function is affected by both `ps` and `state`.
-}
type Op ps state m a o i = Operate (StateT state m) (At a (o :: ps)) (i :: ps)

newtype GenMsg ps state event from
  = GenMsg (state -> event -> Maybe (SomeMsg ps from))

type State2GenMsg ps state event = DMap (Sing @ps) (GenMsg ps state event)

data SomeMsg ps from
  = forall (to :: ps).
    (SingI to) =>
    SomeMsg (Msg ps from to)

type SomeOp ps state m a = SomeOperate ps (StateT state m) a

sOrdToGCompare
  :: forall n (a :: n) (b :: n)
   . (SOrd n)
  => Sing a -> Sing b -> GOrdering a b
sOrdToGCompare a b = case sCompare a b of
  SEQ -> unsafeCoerce GEQ
  SLT -> GLT
  SGT -> GGT

runOp
  :: forall ps event state m a (input :: ps) (output :: ps)
   . ( SingI input
     , GCompare (Sing @ps)
     )
  => (Monad m)
  => State2GenMsg ps state event
  -> [event]
  -> Operate (StateT state m) (At a output) input
  -> (StateT state m) (Result ps (StateT state m) a)
runOp dmp evns = \case
  IReturn (At a) -> pure (Finish a)
  LiftM m -> m Prelude.>>= runOp dmp evns
  In f -> do
    let singInput = sing @input
    case D.lookup singInput dmp of
      Nothing -> pure (NotMatchGenMsg singInput)
      Just (GenMsg genMsg) -> loop evns
       where
        loop [] = pure $ Cont $ SomeOperate (In f)
        loop (et : evns') = do
          state' <- get
          case genMsg state' et of
            Nothing -> loop evns'
            Just (SomeMsg msg) -> runOp dmp evns' (f msg)
