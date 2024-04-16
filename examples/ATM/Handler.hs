{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler where

import Control.Monad.State
import Data.Data (Proxy (..))
import Data.IFunctor (At (..), IMonad (..))
import qualified Data.IFunctor as I
import Data.Kind
import Data.SR
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Lens.Micro.Mtl (use, (-=))
import Type
import TypedFsm.Core
import TypedFsm.Driver

checkResult
  :: forall (n1 :: N) n
   . (SingI n, Reify n, Less3 n1, SingI n1, Reify n1)
  => Proxy n1
  -> Int
  -> Operate (StateT InternalState IO) CheckPINResult ('CheckPin n)
checkResult _ i = I.do
  At userPin <- liftm $ use pin
  if i == userPin
    then LiftM $ pure (ireturn Correct)
    else LiftM $ pure (ireturn (Incorrect @n1))

checkPinFun
  :: forall (n :: N)
   . (SingI n, Reify n)
  => Int
  -> Operate (StateT InternalState IO) CheckPINResult (CheckPin (n :: N))
checkPinFun i = I.do
  case sing @n of
    (SZ :: SN n1) -> checkResult @n1 Proxy i
    (SS SZ :: SN n1) -> checkResult @n1 Proxy i
    (SS (SS SZ) :: SN n1) -> checkResult @n1 Proxy i
    _ -> LiftM $ do
      liftIO $ putStrLn "-> test 3 times, eject card!"
      pure (ireturn EjectCard)

readyHandler :: Op ATMSt InternalState Ready Ready
readyHandler = I.do
  msg <- getInput
  case msg of
    InsertCard -> cardInsertedHandler

cardInsertedHandler
  :: (SingI n, Reify n)
  => Op ATMSt InternalState Ready (CardInserted n)
cardInsertedHandler = I.do
  msg <- getInput
  case msg of
    CIEject -> I.do
      readyHandler
    CheckPIN i -> I.do
      res <- checkPinFun i
      case res of
        EjectCard -> readyHandler
        Incorrect -> cardInsertedHandler
        Correct -> sessionHandler

sessionHandler :: Op ATMSt InternalState Ready Session
sessionHandler = I.do
  msg <- getInput
  case msg of
    GetAmount -> I.do
      liftm $ do
        am <- use amount
        liftIO $ putStrLn ("-> User Amount: " <> show am)
      sessionHandler
    Dispense i -> I.do
      liftm $ do
        amount -= i -- need to check amount is enough?
        liftIO $ putStrLn ("-> Use dispense " ++ show i)
        am <- use amount
        liftIO $ putStrLn ("-> Now User Amount: " <> show am)
      sessionHandler
    SEject -> readyHandler
