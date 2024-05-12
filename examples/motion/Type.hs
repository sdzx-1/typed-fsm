{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Type where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad.State
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.IFunctor (At (..), Sing, SingI)
import qualified Data.IFunctor as I
import Data.Int (Int32)
import Data.Kind
import GHC.Event
import Lens.Micro.Mtl
import Lens.Micro.TH
import SDL
import TypedFsm.Core
import TypedFsm.Driver

----------------------------------
type Point' = Point V2 Int

pattern Point x y = P (V2 x y)
{-# COMPLETE Point #-}

data MyEvent
  = MyMouseMotion (Point V2 Int32) Timestamp
  | MyTimeout
  | MyQuit
  deriving (Show, Eq, Ord)

----------------------------------
data Motion
  = Idle
  | Over
  | Hover
  | Exit
  deriving (Show)

data SMotion :: Motion -> Type where
  SIdel :: SMotion Idle
  SOver :: SMotion Over
  SHover :: SMotion Hover
  SExit :: SMotion Exit

smTom :: SMotion m -> Motion
smTom = \case
  SIdel -> Idle
  SOver -> Over
  SHover -> Hover
  SExit -> Exit

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

type instance Sing = SMotion

instance SingI Idle where
  sing = SIdel

instance SingI Over where
  sing = SOver

instance SingI Hover where
  sing = SHover

instance SingI Exit where
  sing = SExit

deriveGEq ''SMotion
deriveGCompare ''SMotion
makeLenses ''MotionState
