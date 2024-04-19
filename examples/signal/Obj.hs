{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Obj where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.Dynamic
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Data.IORef
import Data.Int (Int32)
import Data.IntMap
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Kind
import Data.SR
import Data.Void
import SDL
import TypedFsm.Core
import TypedFsm.Driver
import TypedFsm.Signal
import Utils

data Obj a
  = Enter
  | Exit

-------------------------------

instance (SignalAndSlot a) => SignalAndSlot (Obj a) where
  type Signal (Obj a) = Signal a
  type Slot (Obj a) = Slot a

-------------------------------
data SObj :: Obj a -> Type where
  SEnter :: SObj Enter
  SExit :: SObj Exit

deriveGEq ''SObj
deriveGCompare ''SObj

type instance Sing = SObj

instance SingI Enter where
  sing = SEnter

instance SingI Exit where
  sing = SExit

instance Reify Enter where
  reifyProxy _ = Enter

instance Reify Exit where
  reifyProxy _ = Exit

instance (SignalAndSlot (Obj a)) => StateTransMsg (Obj a) where
  data Msg (Obj a) from to where
    MouseLeftClick :: Point' -> Msg (Obj a) Enter Enter
    ObjMsg :: (Slot (Obj a)) -> Msg (Obj a) Enter Enter

objDepMap
  :: forall a state
   . (Typeable (Slot (Obj a)))
  => State2GenMsg (Obj a) state MyEvent
objDepMap =
  D.fromList
    [ SEnter
        :=> GenMsg
          ( \_ event -> case event of
              SDLEvent (MyMouseLeftButtonClick p) ->
                Just $
                  SomeMsg (MouseLeftClick p)
              RecvVal d ->
                Just $
                  SomeMsg $
                    ObjMsg $
                      fromDyn @(Slot (Obj a)) d (error "np")
          )
    ]

newtype Ref ps = Ref Int

type AllObjRefs = IORef (IntMap SomeObj)

data ObjEnv ps = ObjEnv
  { _objNum :: Ref ps
  , _globalEnv :: GlobalEnv
  }

type ObjEff ps s = StateT s (ReaderT (ObjEnv ps) IO)

type ObjAction ps state o i = Operate (ObjEff ps state) (At () (o :: ps)) (i :: ps)

objEmit
  :: (Typeable (Signal (Obj a)))
  => Signal (Obj a)
  -> ObjAction (Obj a) m Enter Enter
objEmit signal = I.do
  liftm $ do
    let s' = [toDyn signal]
    ObjEnv (Ref _objNum) (GlobalEnv _ (GlobalSignalEnv _sendSignals _) _) <- ask
    liftIO $ modifyIORef' _sendSignals (IntMap.insertWith (<>) _objNum s')

sendEventFun :: ([MyEvent] -> [MyEvent]) -> AllObjRefs -> Int -> IO ()
sendEventFun fun allObjRefs i = do
  allObjs <- readIORef allObjRefs
  case IntMap.lookup i allObjs of
    Nothing -> pure ()
    Just (SomeObj a b c d e) -> do
      modifyIORef allObjRefs (IntMap.insert i (SomeObj a b c d (fun e)))

sendSDLEventToObj :: MySDLEvent -> AllObjRefs -> Int -> IO ()
sendSDLEventToObj sdle =
  sendEventFun (\e -> SDLEvent sdle : e)

putSDLEventsToObj :: [MySDLEvent] -> AllObjRefs -> Int -> IO ()
putSDLEventsToObj sdle =
  sendEventFun (\_ -> fmap SDLEvent sdle)

sendDynsToObj :: [Dynamic] -> AllObjRefs -> Int -> IO ()
sendDynsToObj dys =
  sendEventFun (\e -> e <> fmap RecvVal dys)

type DrawObj ps state = DrawEnv -> Rect -> Maybe ps -> state -> IO ()

data SomeObj = forall a state. (Typeable (Slot (Obj a))) => SomeObj
  { _objNumber :: Ref (Obj a)
  , _objState :: state
  , _objHandler :: Maybe (SomeOperate (Obj a) (ObjEff (Obj a) state) ())
  , _drawObj :: DrawObj (Obj a) state
  , _objEvnts :: [MyEvent]
  }

drawSomeObjRef :: DrawEnv -> AllObjRefs -> Int -> Rect -> IO ()
drawSomeObjRef de allObjRefs i rect = do
  allObjs <- readIORef allObjRefs
  case IntMap.lookup i allObjs of
    Nothing -> pure ()
    Just (SomeObj _ st _ df _) -> df de rect Nothing st

stepSomeObj :: GlobalEnv -> SomeObj -> IO SomeObj
stepSomeObj ge sobj@(SomeObj _ _ Nothing _ _) = pure sobj
stepSomeObj ge (SomeObj i st (Just (SomeOperate handler)) draw events) = do
  let pp = runOp objDepMap (reverse events) handler
  (res, newSt) <- runReaderT (runStateT pp st) (ObjEnv i ge)
  pure $ SomeObj i newSt (either Just (const Nothing) res) draw []

data GlobalEnv = GlobalEnv
  { _num :: IORef Int
  , _signalEnv :: GlobalSignalEnv
  , _allObjs :: AllObjRefs
  }

type GlobalEff = ReaderT GlobalEnv IO

newObj
  :: (Typeable (Slot (Obj a)))
  => state
  -> SomeOperate (Obj a) (ObjEff (Obj a) state) ()
  -> DrawObj (Obj a) state
  -> GlobalEff (Ref (Obj a))
newObj st sop draw = do
  GlobalEnv nr _ _allObjs <- ask
  liftIO $ do
    i <- readIORef nr
    modifyIORef' nr (+ 1)
    modifyIORef _allObjs (IntMap.insert i (SomeObj (Ref i) st (Just sop) draw []))
    pure (Ref i)

connectTwoObjs
  :: ( SignalAndSlot ps1
     , SignalAndSlot ps2
     , Signal ps1 ~ Slot ps2
     )
  => Ref ps1
  -> Ref ps2
  -> GlobalEff ()
connectTwoObjs (Ref i1) (Ref i2) = do
  GlobalEnv _ (GlobalSignalEnv _ stsRef) _ <- ask
  liftIO $ modifyIORef' stsRef (IntMap.insertWith Set.union i1 (Set.singleton i2))
