{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type where

import Control.Monad.State
import Data.Data (Proxy (..))
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.IFunctor (IMonad (..))
import Data.Singletons (Sing, SingI(..))
import qualified Data.IFunctor as I
import Data.Int (Int32)
import Data.Kind
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Lens.Micro.TH (makeLenses)
import SDL
import TypedFsm.Core
import TypedFsm.Driver

----------------------------------
type Point' = Point V2 Int

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

data N = Z | S N
  deriving (Show)

data SN :: N -> Type where
  SZ :: SN Z
  SS :: SN n -> SN (S n)

snTon :: SN n -> N
snTon SZ = Z
snTon (SS s1) = S (snTon s1)

satmToatm :: SATMSt s -> ATMSt
satmToatm = \case
  SReady -> Ready
  SCardInserted sn -> CardInserted (snTon sn)
  SCheckPin sn -> CheckPin (snTon sn)
  SSession -> Session

data ATMSt
  = Ready
  | CardInserted N
  | CheckPin N
  | Session
  | Exit
  deriving (Show)

data SATMSt :: ATMSt -> Type where
  SReady :: SATMSt Ready
  SCardInserted :: SN n -> SATMSt (CardInserted n)
  SCheckPin :: SN n -> SATMSt (CheckPin n)
  SSession :: SATMSt Session
  SExit :: SATMSt Exit

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

type instance Sing = SATMSt

instance SingI Ready where
  sing = SReady

instance (SingI t) => SingI (CardInserted t) where
  sing = SCardInserted sing

instance (SingI t) => SingI (CheckPin t) where
  sing = SCheckPin sing

instance SingI Session where
  sing = SSession

instance SingI Exit where
  sing = SExit

type instance Sing = SN

instance SingI Z where
  sing = SZ

instance (SingI n) => SingI (S n) where
  sing = SS sing

deriveGEq ''SN
deriveGCompare ''SN

deriveGEq ''SATMSt
deriveGCompare ''SATMSt
makeLenses ''Rect
makeLenses ''Label
makeLenses ''InternalState
