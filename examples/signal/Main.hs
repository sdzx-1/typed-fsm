{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State (StateT (runStateT))
import qualified Control.Monad.State as M
import Data.Data (Proxy (..))
import Data.IFunctor (IMonad (..))
import qualified Data.IFunctor as I
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as Set
import Data.Kind
import Data.SR
import Data.Typeable
import Data.Void
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Handler
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (.=))
import Obj
import SDL
import qualified SDL.Font as Font
import TypedFsm.Core
import TypedFsm.Driver
import TypedFsm.Signal
import Utils
import Win

main :: IO ()
main = do
  initialize [InitVideo]
  Font.initialize
  font <- Font.load "data/fonts/SourceCodePro-Regular.otf" 20
  window <- createWindow "test" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ccref <- initCharCache

  numRef <- newIORef 0
  sref <- newIORef IntMap.empty
  sref1 <- newIORef IntMap.empty
  allObjRefs <- newIORef IntMap.empty

  let globalEnv = GlobalEnv numRef (GlobalSignalEnv sref sref1) allObjRefs

  runReaderT
    (initWin >>= appLoop (DrawEnv renderer font ccref))
    globalEnv

  destroyWindow window

initWin :: GlobalEff (Ref (Obj Win))
initWin = do
  (pbInc, buttonInc) <- newPod (Rect 10 10 100 30) "IL" (SomeOperate buttonOp) drawButton
  (pb, button) <- newPod (Rect 130 10 100 30) "IN" (SomeOperate buttonOp) drawButton
  (pm, myst) <- newPod (Rect 10 50 0 0) 0 (SomeOperate myStOp) drawMySt
  connectTwoObjs button myst
  GlobalEnv _ _ allObjRefs <- ask
  winRef <-
    newObj
      (WinState [pbInc, pb, pm] myst 0)
      (SomeOperate winOp)
      (drawWin allObjRefs)
  connectTwoObjs buttonInc winRef
  pure winRef

stepGlobal :: Ref (Obj Win) -> GlobalEff ()
stepGlobal (Ref i) = do
  events <- pollEvents
  let myEvents' = makeMyEvent' events
  GlobalEnv _ globalSignalEnv allObjRefs <- ask
  liftIO $ putSDLEventsToObj myEvents' allObjRefs i

  -- dispatch Obj msg
  GlobalEnv _ gse@(GlobalSignalEnv sendsRef s2sRef) _ <- ask
  sends <- liftIO $ readIORef sendsRef
  liftIO $ writeIORef sendsRef IntMap.empty
  s2s <- liftIO $ readIORef s2sRef
  forM_ (IntMap.toList sends) $ \(pi, dys) -> do
    case IntMap.lookup pi s2s of
      Nothing -> pure ()
      Just sets -> forM_ (Set.toList sets) $ \si -> do
        liftIO $ sendDynsToObj dys allObjRefs si

  -- unpdate all Objs
  allObjs <- IntMap.toList <$> liftIO (readIORef allObjRefs)
  forM_ allObjs $ \(i, sobj) -> do
    ge <- ask
    nobj <- liftIO $ stepSomeObj ge sobj
    liftIO $ modifyIORef allObjRefs (IntMap.insert i nobj)

----------------------------------------------------
appLoop :: DrawEnv -> Ref (Obj Win) -> GlobalEff ()
appLoop de@(DrawEnv renderer font ccref) ref@(Ref i) = do
  -- handl event, obj msg
  stepGlobal ref

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  -- draw Win
  rendererDrawColor renderer $= V4 255 255 255 255
  GlobalEnv _ _ allObjRefs <- ask
  liftIO $ drawSomeObjRef de allObjRefs i (Rect 0 0 800 600)

  present renderer
  liftIO $ threadDelay (1000000 `div` 30)

  appLoop de ref
