{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedFsm.Signal where

import Data.Kind

class SignalAndSlot (ps :: Type) where
  type Signal ps :: Type
  type Slot ps :: Type
