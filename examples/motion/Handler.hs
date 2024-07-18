{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Handler where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad.State
import Data.IFunctor (At (..), returnAt)
import qualified Data.IFunctor as I
import GHC.Event
import Lens.Micro.Mtl
import SDL
import Type
import TypedFsm.Core
import TypedFsm.Driver

timeoutSize :: Int
timeoutSize = 400_000

myRegisterTimeout :: StateT MotionState IO (TimerManager, TimeoutKey)
myRegisterTimeout = do
  chan <- use channel
  liftIO $ do
    tm <- getSystemTimerManager
    tk <- registerTimeout tm timeoutSize (atomically $ writeTChan chan ())
    pure (tm, tk)

idelHandler :: Op Motion MotionState IO Exit Idle
idelHandler = I.do
  msg <- getInput
  case msg of
    ExitMotion -> returnAt ()
    MoveIn pos tms -> I.do
      At tp <- liftm $ do
        mousePos .= pos
        (tm, tk) <- myRegisterTimeout
        pure (tm, tk, tms)
      overHandler tp

overHandler :: (TimerManager, TimeoutKey, Timestamp) -> Op Motion MotionState IO Exit Over
overHandler (tm, tk, oldtms) = I.do
  msg <- getInput
  case msg of
    ExitMotion -> returnAt ()
    MoveOut -> I.do
      liftm $ liftIO $ unregisterTimeout tm tk
      idelHandler
    InMove pos tms -> I.do
      liftm $ do
        mousePos .= pos
      if tms - oldtms > 40
        then I.do
          liftm $ liftIO $ updateTimeout tm tk timeoutSize
          overHandler (tm, tk, tms)
        else overHandler (tm, tk, oldtms)
    TimeoutH -> I.do
      liftm $ do
        liftIO $ unregisterTimeout tm tk
        pos <- use mousePos
        onHover .= Just (pos, [show oldtms, show pos])
      hoverHandler

hoverHandler :: Op Motion MotionState IO Exit Hover
hoverHandler = I.do
  msg <- getInput
  case msg of
    ExitMotion -> returnAt ()
    HInMove pos tms -> I.do
      At tp <- liftm $ do
        mousePos .= pos
        onHover .= Nothing
        (tm, tk) <- myRegisterTimeout
        pure (tm, tk, tms)
      overHandler tp
    HMoveOut -> I.do
      liftm $ onHover .= Nothing
      idelHandler
