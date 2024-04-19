{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Win where

import Control.Monad (forM_)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as M
import Data.Dynamic
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Data.IORef
import Data.Int (Int32)
import Data.IntMap
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Kind
import Data.Void
import Handler
import Lens.Micro.GHC
import Lens.Micro.Mtl (use, (%=), (.=), (?=))
import Lens.Micro.TH
import Obj
import SDL
import TypedFsm.Core
import TypedFsm.Driver
import TypedFsm.Signal
import Utils

data Pod = Pod
  { _podRect :: Rect
  , _someObjIndex :: Int
  }

makeLenses ''Pod

data Win

instance SignalAndSlot Win where
  type Signal Win = Void
  type Slot Win = ()

data WinState = WinState
  { _allPods :: [Pod]
  , _myStRef :: Ref (Obj MySt)
  , _index :: Int
  }
makeLenses ''WinState

findSFirst :: (a -> b -> Bool) -> a -> [b] -> Maybe b
findSFirst _ _ [] = Nothing
findSFirst f a (va : xs) =
  if f a va
    then Just va
    else findSFirst f a xs

winOp :: ObjAction (Obj Win) WinState Exit Enter
winOp = I.do
  msg <- getInput
  case msg of
    MouseLeftClick pos -> I.do
      liftm $ do
        all <- use allPods
        case findSFirst (\p (Pod rect _) -> rect `contains` p) pos all of
          Just (Pod _ i) -> do
            ObjEnv _ (GlobalEnv _ _ allObjRefs) <- ask
            liftIO $ sendSDLEventToObj (MyMouseLeftButtonClick pos) allObjRefs i
          Nothing -> pure ()
      winOp
    ObjMsg _ -> I.do
      liftm $ do
        ObjEnv _ ge <- ask
        WinState allPods myRef index <- M.get
        let fun = newPod (Rect 10 (80 + index * 33) 100 30) "null" (SomeOperate labelOp) drawLabel
        (pl1, label1) <- liftIO $ runReaderT fun ge
        liftIO $ runReaderT (connectTwoObjs myRef label1) ge
        put (WinState (pl1 : allPods) myRef (index + 1))
      winOp

newPod
  :: (Typeable (Slot (Obj a)))
  => Rect
  -> state
  -> SomeOperate (Obj a) (ObjEff (Obj a) state) ()
  -> DrawObj (Obj a) state
  -> GlobalEff (Pod, Ref (Obj a))
newPod rect st sop draw = do
  o@(Ref i) <- newObj st sop draw
  pure (Pod rect i, o)

drawWin :: AllObjRefs -> DrawObj (Obj Win) WinState
drawWin allObjRefs de _ _ (WinState pods _ _) = do
  forM_ pods $ \(Pod rect i) -> drawSomeObjRef de allObjRefs i rect
