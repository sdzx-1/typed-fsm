{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (runStateT))
import qualified Control.Monad.State as M
import Data.Singletons (SomeSing (..))
import EventToMsg
import Handler
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
  chan <- newTChanIO @()

  runStateT
    (appLoop (DrawEnv renderer font ccref) chan mouseDepMap (SomeOperate SIdle idelHandler))
    (MotionState (Rect 100 100 400 400) chan (Point 0 0) Nothing)

  destroyWindow window

creatRect :: (Integral p1, Integral p2, Integral p3, Integral p4, Num a) => p1 -> p2 -> p3 -> p4 -> Rectangle a
creatRect x y w h =
  Rectangle
    (P (V2 (fromIntegral x) (fromIntegral y)))
    (V2 (fromIntegral w) (fromIntegral h))

appLoop
  :: DrawEnv
  -> TChan ()
  -> State2GenMsg Motion MotionState MyEvent
  -> SomeOp Motion MotionState IO ()
  -> StateT MotionState IO ()
appLoop de@(DrawEnv renderer _font _ccref) chan depMap (SomeOperate sinput fun) = do
  events <- pollEvents
  ma <- liftIO (atomically $ tryReadTChan chan)
  v <- runOp depMap (makeMyEvent ma events) sinput fun
  case v of
    Finish _ -> pure ()
    ErrorInfo (NotFoundGenMsg (SomeSing si)) -> liftIO $ putStrLn $ "error: not match GenMsg " ++ show si
    Cont fun1 -> do
      rendererDrawColor renderer $= V4 0 0 0 255
      clear renderer

      let motionState = getSomeOperateSt fun1
      liftIO $ drawStrings de [show motionState] (10, 30)

      MotionState (Rect x y w h) _ _ mOnhove <- M.get
      rendererDrawColor renderer $= V4 255 0 155 255
      drawRect renderer $ Just (creatRect x y w h)

      liftIO $
        case mOnhove of
          Nothing -> pure ()
          Just (Point x' y', st) -> do
            rendererDrawColor renderer $= V4 100 155 144 255
            fillRect renderer $ Just (creatRect x' y' (300 :: Int) (-100 :: Int))
            drawStrings de st (fromIntegral x', fromIntegral y' - 60)

      present renderer
      liftIO $ threadDelay (1000000 `div` 30)

      appLoop de chan depMap fun1
