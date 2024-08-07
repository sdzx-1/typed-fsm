{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Type where

import Control.Concurrent.STM.TChan
import Data.GADT.Compare (GCompare (..), GEq (..))
import Data.Int (Int32)
import Data.Singletons.Base.TH
import Data.Type.Equality (TestEquality (testEquality))
import Lens.Micro.TH
import SDL
import TypedFsm

$( singletons
    [d|
      data Motion
        = Idle
        | Over
        | Hover
        | Exit
        deriving (Show, Eq, Ord)
      |]
 )

instance GEq SMotion where
  geq = testEquality

instance GCompare SMotion where
  gcompare = sOrdToGCompare

smTom :: SMotion m -> Motion
smTom = fromSing

----------------------------------
type Point' = Point V2 Int

pattern Point :: a -> a -> Point V2 a
pattern Point x y = P (V2 x y)
{-# COMPLETE Point #-}

instance StateTransMsg Motion where
  data Msg Motion from to where
    ExitMotion :: Msg Motion s Exit
    -----------------
    MoveIn :: Point' -> Timestamp -> Msg Motion Idle Over
    -----------------
    MoveOut :: Msg Motion Over Idle
    InMove :: Point' -> Timestamp -> Msg Motion Over Over
    TimeoutH :: Msg Motion Over Hover
    -----------------
    HInMove :: Point' -> Timestamp -> Msg Motion Hover Over
    HMoveOut :: Msg Motion Hover Idle

data Rect = Rect
  { _rx :: Int
  , _ry :: Int
  , _width :: Int
  , _height :: Int
  }
  deriving (Show)

data MotionState = MotionState
  { _rect :: Rect
  , _channel :: TChan ()
  , _mousePos :: Point'
  , _onHover :: Maybe (Point', [String])
  }

data MyEvent
  = MyMouseMotion (Point V2 Int32) Timestamp
  | MyTimeout
  | MyQuit
  deriving (Show, Eq, Ord)

----------------------------------

makeLenses ''MotionState
