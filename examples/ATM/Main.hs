{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use print" #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (runStateT))
import qualified Control.Monad.State as M
import Data.Data (Proxy (..))
import Data.IFunctor (IMonad (..))
import qualified Data.IFunctor as I
import Data.Kind
import EventToMsg
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Handler
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((.=))
import SDL
import qualified SDL.Font as Font
import Type
import TypedFsm.Core
import TypedFsm.Driver
import Utils

main :: IO ()
main = do
  initialize [InitVideo]
  Font.initialize
  font <- Font.load "data/fonts/SourceCodePro-Regular.otf" 20
  window <- createWindow "test" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ccref <- initCharCache

  runStateT
    (appLoop (DrawEnv renderer font ccref) (SomeOperate readyHandler))
    initInternState
  destroyWindow window

appLoop :: DrawEnv -> SomeOp ATMSt InternalState IO -> StateT InternalState IO ()
appLoop de@(DrawEnv renderer font ccref) (SomeOperate fun) = do
  events <- pollEvents
  -- liftIO $ print events
  v <- runOp atmDepMap (makeMyEvent events) fun
  case v of
    Left fun1 -> do
      let atmSt = satmToatm $ singSomeOperate fun1
      rendererDrawColor renderer $= V4 0 0 0 255
      clear renderer

      rendererDrawColor renderer $= V4 255 0 155 255
      liftIO $ drawStrings de [show atmSt] (10, 30)

      ist <- M.get
      case atmSt of
        Ready -> do
          liftIO $ drawLabel de (ist ^. insCardLabel)
          liftIO $ drawLabel de (ist ^. exitLabel)
        CardInserted n -> do
          liftIO $ drawLabel de (ist ^. checkPinLabel)
          liftIO $ drawLabel de (ist ^. checkPinErrorLabel)
          liftIO $ drawLabel de (ist ^. ejectLabel)
        Session -> do
          liftIO $ drawLabel de (ist ^. amountLabel)
          liftIO $ drawLabel de (ist ^. getAmountLabel)
          liftIO $ drawLabel de (ist ^. dispenseLabel)
          liftIO $ drawLabel de (ist ^. ejectLabel)
        CheckPin n -> pure ()

      present renderer
      liftIO $ threadDelay (1000000 `div` 30)
      appLoop de fun1
    Right _ -> pure ()
