{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}

module Handler where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import qualified Control.Monad.State as M
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Data.Void
import Obj
import SDL
import TypedFsm.Core
import TypedFsm.Driver
import TypedFsm.Signal
import Utils

-------------------------------

drawButton :: DrawObj (Obj Button) String
drawButton de rect@(Rect x y _ _) ps string = do
  drawString de string (x, y)
  drawRect' de rect

drawLabel :: DrawObj (Obj Label) String
drawLabel de rect@(Rect x y _ _) ps string = do
  drawString de string (x, y)
  drawRect' de rect

drawMySt :: DrawObj (Obj MySt) Int
drawMySt _ _ _ _ = pure ()

myStOp :: ObjAction (Obj MySt) Int Exit Enter
myStOp = I.do
  msg <- getInput
  case msg of
    MouseLeftClick _ -> myStOp
    ObjMsg v -> I.do
      At nv <- liftm $ do
        liftIO $ putStrLn ("mySt recv: " <> show v)
        modify' (+ 1)
        M.get
      objEmit ("now val: " <> show nv)
      liftm $ liftIO $ putStrLn ("mySt send: new val: " <> show nv)
      myStOp

labelOp :: ObjAction (Obj Label) String Exit Enter
labelOp = I.do
  msg <- getInput
  case msg of
    MouseLeftClick _ -> labelOp
    ObjMsg v -> I.do
      liftm $ do
        liftIO $ putStrLn ("Label recv: " <> v)
        put v
      labelOp

buttonOp :: ObjAction (Obj Button) String Exit Enter
buttonOp = I.do
  msg <- getInput
  case msg of
    MouseLeftClick _ -> I.do
      objEmit ()
      liftm $ liftIO $ putStrLn ("Button send: ()")
      buttonOp
    ObjMsg _v -> buttonOp
