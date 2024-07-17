{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Type where

import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Int (Int32)
import Data.Kind
import Data.Singletons.Base.TH
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Lens.Micro.TH (makeLenses)
import SDL
import TypedFsm.Core
import Unsafe.Coerce (unsafeCoerce)

----------------------------------
type Point' = Point V2 Int

pattern Point :: a -> a -> Point V2 a
pattern Point x y = P (V2 x y)
{-# COMPLETE Point #-}

newtype MyEvent = MyMouseLeftButtonClick (Point V2 Int32)
  deriving (Show, Eq, Ord)

data Rect = Rect
  { _rx :: Int
  , _ry :: Int
  , _width :: Int
  , _height :: Int
  }
  deriving (Show)

data Label = Label
  { _rect :: Rect
  , _label :: String
  }
  deriving (Show)

----------------------------------

$( singletons
    [d|
      data N = Z | S N
        deriving (Show)

      data ATMSt
        = Ready
        | CardInserted N
        | CheckPin N
        | Session
        | Exit
        deriving (Show)
      |]
 )

snTon :: SN n -> N
snTon SZ = Z
snTon (SS s1) = S (snTon s1)

satmToatm :: SATMSt s -> ATMSt
satmToatm = \case
  SReady -> Ready
  SCardInserted sn -> CardInserted (snTon sn)
  SCheckPin sn -> CheckPin (snTon sn)
  SSession -> Session
  SExit -> Exit

-- deriveGEq ''SN
instance GEq SN where
  geq SZ SZ = Just Refl
  geq (SS a) (SS b) = do
    v <- geq a b
    pure $ unsafeCoerce v
  geq _ _ = Nothing

-- deriveGCompare ''SN
instance GCompare SN where
  gcompare SZ SZ = GEQ
  gcompare (SS _) SZ = GGT
  gcompare SZ (SS _) = GLT
  gcompare (SS a) (SS b) = unsafeCoerce $ gcompare a b

-- deriveGEq ''SATMSt
instance GEq SATMSt where
  geq SReady SReady = Just Refl
  geq (SCardInserted a) (SCardInserted b) = do
    v <- geq a b
    pure $ unsafeCoerce v
  geq (SCheckPin a) (SCheckPin b) = do
    v <- geq a b
    pure $ unsafeCoerce v
  geq SSession SSession = Just Refl
  geq SExit SExit = Just Refl
  geq _ _ = Nothing

-- deriveGCompare ''SATMSt
instance GCompare SATMSt where
  gcompare SReady SReady = GEQ
  gcompare SReady _ = GLT
  gcompare (SCardInserted _) SReady = GGT
  gcompare (SCardInserted a) (SCardInserted b) = unsafeCoerce $ gcompare a b
  gcompare (SCardInserted _) _ = GLT
  gcompare (SCheckPin _) SReady = GGT
  gcompare (SCheckPin _) (SCardInserted _) = GGT
  gcompare (SCheckPin a) (SCheckPin b) = unsafeCoerce $ gcompare a b
  gcompare (SCheckPin _) _ = GLT
  gcompare SSession SReady = GGT
  gcompare SSession (SCardInserted _) = GGT
  gcompare SSession (SCheckPin _) = GGT
  gcompare SSession SSession = GEQ
  gcompare SSession _ = GLT
  gcompare SExit SReady = GGT
  gcompare SExit (SCardInserted _) = GGT
  gcompare SExit (SCheckPin _) = GGT
  gcompare SExit SSession = GGT
  gcompare SExit SExit = GEQ

type family Less3 (n :: N) :: Constraint where
  Less3 Z = ()
  Less3 (S Z) = ()
  Less3 (S (S Z)) = ()
  Less3 _ = TypeError (Text "test must less 3")

data CheckPINResult :: ATMSt -> Type where
  EjectCard :: (n ~ S (S (S Z))) => SN n -> CheckPINResult Ready
  Incorrect :: (SingI n, Less3 n) => CheckPINResult (CardInserted n)
  Correct :: CheckPINResult Session

instance StateTransMsg ATMSt where
  data Msg ATMSt from to where
    ExitATM :: Msg ATMSt Ready Exit
    InsertCard :: Msg ATMSt Ready (CardInserted Z)
    CIEject :: Msg ATMSt (CardInserted n) Ready
    ---------------
    CheckPIN :: Int -> Msg ATMSt (CardInserted n) (CheckPin (S n))
    ---------------
    GetAmount :: Msg ATMSt Session Session
    Dispense :: Int -> Msg ATMSt Session Session
    SEject :: Msg ATMSt Session Ready

data InternalState = InternalState
  { _pin :: Int
  , _amount :: Int
  , _amountLabel :: Label
  , _insCardLabel :: Label
  , _exitLabel :: Label
  , _checkPinLabel :: Label
  , _checkPinErrorLabel :: Label
  , _getAmountLabel :: Label
  , _dispenseLabel :: Label
  , _ejectLabel :: Label
  }
  deriving (Show)

initInternState :: InternalState
initInternState =
  InternalState
    { _pin = 1234
    , _amount = 1000
    , _amountLabel = Label (Rect 10 130 100 30) "null"
    , _insCardLabel = (Label (Rect 10 230 100 30) "Insert Card")
    , _exitLabel = (Label (Rect 10 270 100 30) "EXIT")
    , _checkPinLabel = (Label (Rect 10 230 100 30) "checkPin 1234")
    , _checkPinErrorLabel = (Label (Rect 10 274 100 30) "checkPin 1 error")
    , _getAmountLabel = (Label (Rect 10 230 100 30) "GetAmount")
    , _dispenseLabel = (Label (Rect 130 230 100 30) "Dispense 100")
    , _ejectLabel = (Label (Rect 10 330 100 30) "Eject")
    }

makeLenses ''Rect
makeLenses ''Label
makeLenses ''InternalState
