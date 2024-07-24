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
import Data.Singletons (Sing)
import TypedFsm.Core (Operate (..), StateTransMsg (Msg))
import TypedFsm.Driver.Common
import Unsafe.Coerce (unsafeCoerce)

anyToSomeMsg
  :: forall ps input
   . (SEq ps)
  => Sing input -> AnyMsg ps -> Maybe (SomeMsg ps input)
anyToSomeMsg sinput (AnyMsg sfrom sto (msg :: Msg ps from to)) =
  case sfrom %== sinput of
    -- (from == input) ~ True
    -- ==> from ~ input
    STrue -> unsafeCoerce (Just (SomeMsg sto msg))
    SFalse -> Nothing

newtype UnexpectMsg ps = UnexpectMsg (AnyMsg ps)

data UnexpectMsgHandler ps m
  = Ignore
  | IgnoreAndTrace (AnyMsg ps -> m ())
  | Terminal

runOperate
  :: forall ps m a (input :: ps) (output :: ps)
   . ( Monad m
     , SEq ps
     )
  => UnexpectMsgHandler ps m
  -> [AnyMsg ps]
  -> Sing input
  -> Operate m (At a output) input
  -> m (Result ps (UnexpectMsg ps) m a)
runOperate unHandler anyMsgs sinput = \case
  IReturn (At a) -> pure (Finish a)
  LiftM singv m -> m >>= (runOperate unHandler anyMsgs singv)
  In f -> loop anyMsgs
   where
    loop [] = pure $ Cont $ SomeOperate sinput (In f)
    loop (anyMsg : evns') = do
      case anyToSomeMsg sinput anyMsg of
        Nothing -> case unHandler of
          Ignore -> loop evns'
          IgnoreAndTrace trace -> trace anyMsg >> loop evns'
          Terminal -> pure (ErrorInfo $ UnexpectMsg anyMsg)
        Just (SomeMsg sto msg) -> runOperate unHandler evns' sto (f msg)
