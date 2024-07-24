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

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (runStateT))
import qualified Control.Monad.State as M
import Data.Singletons (SomeSing (SomeSing))
import EventToMsg
import Handler
import Lens.Micro ((^.))
import SDL
import qualified SDL.Font as Font
import Type
import TypedFsm
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
    (appLoop (DrawEnv renderer font ccref) (SomeOperate SReady readyHandler))
    initInternState
  destroyWindow window

appLoop :: DrawEnv -> SomeOp ATMSt InternalState IO () -> StateT InternalState IO ()
appLoop de@(DrawEnv renderer _font _ccref) (SomeOperate sinput fun) = do
  events <- pollEvents
  -- liftIO $ print events
  v <- runOp atmDepMap (makeMyEvent events) sinput fun
  case v of
    Finish _ -> pure ()
    ErrorInfo (NotFoundGenMsg (SomeSing si)) ->
      liftIO $ putStrLn $ "error: not match GenMsg " ++ show si
    Cont fun1 -> do
      let atmSt = getSomeOperateSt fun1
      rendererDrawColor renderer $= V4 0 0 0 255
      clear renderer

      rendererDrawColor renderer $= V4 255 0 155 255
      liftIO $ drawStrings de [show atmSt] (10, 30)

      ist <- M.get
      case atmSt of
        Ready -> do
          liftIO $ drawLabel de (ist ^. insCardLabel)
          liftIO $ drawLabel de (ist ^. exitLabel)
        CardInserted _n -> do
          let ls = ist ^. tmpPin
          liftIO $ drawLabel de (Label (Rect 10 90 300 30) (show $ reverse ls))
          liftIO $ drawLabel de (ist ^. ejectLabel)
          liftIO $ forM_ (ist ^. nlabels) $ drawNLabel de
        Session -> do
          liftIO $ drawLabel de (ist ^. amountLabel)
          liftIO $ drawLabel de (ist ^. getAmountLabel)
          liftIO $ drawLabel de (ist ^. dispenseLabel)
          liftIO $ drawLabel de (ist ^. ejectLabel)
        CheckPin _n -> pure ()
        Exit -> pure ()

      present renderer
      liftIO $ threadDelay (1000000 `div` 30)
      appLoop de fun1
