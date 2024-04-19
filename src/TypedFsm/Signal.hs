{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedFsm.Signal where

import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import Data.IFunctor (At (..))
import Data.IFunctor qualified as I
import Data.IORef
import Data.IntMap
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.Kind
import Data.SR
import Data.Void
import TypedFsm.Core
import TypedFsm.Driver

class SignalAndSlot (ps :: Type) where
  type Signal ps :: Type
  type Slot ps :: Type
