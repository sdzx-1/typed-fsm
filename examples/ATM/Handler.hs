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
import Data.IFunctor (At (..), IMonad (..), returnAt)
import qualified Data.IFunctor as I
import Data.Singletons (SingI (sing))
import Lens.Micro.Mtl (use, (%=), (-=), (.=))
import Type
import TypedFsm

checkResult
  :: forall n
   . (SingI n, Less3 n)
  => [Int]
  -> Operate (StateT InternalState IO) CheckPINResult (CheckPin n)
checkResult i = I.do
  At userPin <- liftm $ use pin
  if i == userPin
    then liftConstr Correct
    else liftConstr (Incorrect @n)

checkPinFun
  :: forall (n :: N)
   . (SingI n)
  => [Int]
  -> Operate (StateT InternalState IO) CheckPINResult (CheckPin (n :: N))
checkPinFun i = I.do
  case sing @n of
    SS SZ -> checkResult i
    SS (SS SZ) -> checkResult i
    sn@(SS (SS (SS SZ))) -> I.do
      At userPin <- liftm $ use pin
      if i == userPin
        then liftConstr Correct
        else I.do
          liftm $ liftIO $ putStrLn "-> test 3 times, eject card!"
          liftConstr (EjectCard sn)
    _ -> error "np"

readyHandler :: Op ATMSt InternalState IO () Exit Ready
readyHandler = I.do
  msg <- getInput
  case msg of
    InsertCard -> I.do
      liftm $ tmpPin .= []
      cardInsertedHandler
    ExitATM -> returnAt ()

removeOne :: [a] -> [a]
removeOne [] = []
removeOne (_ : xs) = xs

cardInsertedHandler
  :: (SingI n)
  => Op ATMSt InternalState IO () Exit (CardInserted n)
cardInsertedHandler = I.do
  msg <- getInput
  case msg of
    CIEject -> readyHandler
    AddNum i -> I.do
      liftm $ tmpPin %= (i :)
      cardInsertedHandler
    DelectNum -> I.do
      liftm $ tmpPin %= removeOne
      cardInsertedHandler
    CheckPIN i -> I.do
      res <- checkPinFun i
      case res of
        EjectCard _ -> readyHandler
        Incorrect -> cardInsertedHandler
        Correct -> I.do
          liftm $ amountLabel . label .= ("Amount: -- ")
          sessionHandler

sessionHandler :: Op ATMSt InternalState IO () Exit Session
sessionHandler = I.do
  msg <- getInput
  case msg of
    GetAmount -> I.do
      liftm $ do
        am <- use amount
        amountLabel . label .= ("Amount: " <> show am)
        liftIO $ putStrLn ("-> User Amount: " <> show am)
      sessionHandler
    Dispense i -> I.do
      liftm $ do
        amount -= i -- need to check amount is enough?
        liftIO $ putStrLn ("-> Use dispense " ++ show i)
        am <- use amount
        amountLabel . label .= ("Now Amount: " <> show am)
        liftIO $ putStrLn ("-> Now User Amount: " <> show am)
      sessionHandler
    SEject -> readyHandler
