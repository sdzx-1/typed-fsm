{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

sOrdToGCompare
  :: forall n (a :: n) (b :: n)
   . (SOrd n)
  => Sing a -> Sing b -> GOrdering a b
sOrdToGCompare a b = case sCompare a b of
  SEQ -> unsafeCoerce GEQ
  SLT -> GLT
  SGT -> GGT

newtype GenMsg ps state event from
  = GenMsg (state -> event -> Maybe (SomeMsg ps from))

type State2GenMsg ps state event = DMap (Sing @ps) (GenMsg ps state event)

data SomeMsg ps from
  = forall (to :: ps).
    (SingI to) =>
    SomeMsg (Msg ps from to)

data SomeOperate ts m a
  = forall (i :: ts) (o :: ts).
    (SingI i) =>
    SomeOperate (Operate m (At a o) i)

getSomeOperateSt :: (SingKind ts) => SomeOperate ts m a -> Demote ts
getSomeOperateSt (SomeOperate (_ :: Operate m (At a o) i)) = fromSing $ sing @i

type OpResult ps m a = (Either (SomeOperate ps m a) a)

type Op ps state m o i = Operate (StateT state m) (At () (o :: ps)) (i :: ps)

type SomeOp ps state m = SomeOperate ps (StateT state m) ()

runOp
  :: forall ps event state m a (input :: ps) (output :: ps)
   . ( SingI input
     , GCompare (Sing @ps)
     )
  => (Monad m)
  => State2GenMsg ps state event
  -> [event]
  -> Operate (StateT state m) (At a output) input
  -> (StateT state m) (OpResult ps (StateT state m) a)
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
