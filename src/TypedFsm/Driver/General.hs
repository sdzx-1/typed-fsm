{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Running FSM
module TypedFsm.Driver.General where

import Data.Bool.Singletons (SBool (..))
import Data.Eq.Singletons (SEq (..))
import Data.IFunctor (At (..))
import Data.Singletons (SingI (..))
import TypedFsm.Core (Operate (..), StateTransMsg (Msg))
import TypedFsm.Driver.Common
import Unsafe.Coerce (unsafeCoerce)

anyToSomeMsg
  :: forall ps input
   . (SingI input, SEq ps)
  => AnyMsg ps -> Maybe (SomeMsg ps input)
anyToSomeMsg (AnyMsg (msg :: Msg ps from to)) =
  case sing @from %== sing @input of
    -- (from == input) ~ True
    -- ==> from ~ input
    STrue -> unsafeCoerce (Just (SomeMsg msg))
    SFalse -> Nothing

newtype UnexpectMsg ps = UnexpectMsg (AnyMsg ps)

data UnexpectMsgHandler ps m
  = Ignore
  | IgnoreAndTrace (AnyMsg ps -> m ())
  | Terminal

runOperate
  :: forall ps m a (input :: ps) (output :: ps)
   . ( Monad m
     , SingI input
     , SEq ps
     )
  => UnexpectMsgHandler ps m
  -> [AnyMsg ps]
  -> Operate m (At a output) input
  -> m (Result ps (UnexpectMsg ps) m a)
runOperate unHandler anyMsgs = \case
  IReturn (At a) -> pure (Finish a)
  LiftM m -> m >>= (runOperate unHandler anyMsgs)
  In f -> loop anyMsgs
   where
    loop [] = pure $ Cont $ SomeOperate (In f)
    loop (anyMsg : evns') = do
      case anyToSomeMsg @_ @input anyMsg of
        Nothing -> case unHandler of
          Ignore -> loop evns'
          IgnoreAndTrace trace -> trace anyMsg >> loop evns'
          Terminal -> pure (ErrorInfo $ UnexpectMsg anyMsg)
        Just (SomeMsg msg) -> runOperate unHandler evns' (f msg)
