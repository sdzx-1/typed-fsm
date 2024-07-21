{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
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

import Data.GADT.Compare (GCompare (..), GEq (..))
import Data.Int (Int32)
import Data.Kind (Constraint, Type)
import Data.Singletons.Base.TH
import Data.Type.Equality (TestEquality (testEquality))
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Lens.Micro.TH (makeLenses)
import SDL
import TypedFsm

$( singletons
    [d|
      data N = Z | S N
        deriving (Show, Eq, Ord)

      data ATMSt
        = Ready
        | CardInserted N
        | CheckPin N
        | Session
        | Exit
        deriving (Show, Eq, Ord)
      |]
 )

satmToatm :: SATMSt s -> ATMSt
satmToatm = fromSing

instance GEq SN where
  geq = testEquality

instance GEq SATMSt where
  geq = testEquality

instance GCompare SN where
  gcompare = sOrdToGCompare

instance GCompare SATMSt where
  gcompare = sOrdToGCompare

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
    AddNum :: Int -> Msg ATMSt (CardInserted n) (CardInserted n)
    DelectNum :: Msg ATMSt (CardInserted n) (CardInserted n)
    CheckPIN :: [Int] -> Msg ATMSt (CardInserted n) (CheckPin (S n))
    ---------------
    GetAmount :: Msg ATMSt Session Session
    Dispense :: Int -> Msg ATMSt Session Session
    SEject :: Msg ATMSt Session Ready

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

data NLabel = NLabel
  { _nrect :: Rect
  , _nlabel :: Int
  }
  deriving (Show)

----------------------------------

data InternalState = InternalState
  { _pin :: [Int]
  , _amount :: Int
  , _amountLabel :: Label
  , _insCardLabel :: Label
  , _exitLabel :: Label
  , _tmpPin :: [Int]
  , _nlabels :: [NLabel]
  , _getAmountLabel :: Label
  , _dispenseLabel :: Label
  , _ejectLabel :: Label
  }
  deriving (Show)

initInternState :: InternalState
initInternState =
  InternalState
    { _pin = [1, 2, 3, 4]
    , _amount = 1000
    , _amountLabel = Label (Rect 10 130 100 30) "null"
    , _insCardLabel = (Label (Rect 10 230 100 30) "Insert Card")
    , _exitLabel = (Label (Rect 10 330 100 30) "EXIT")
    , _tmpPin = []
    , _nlabels = concat [[NLabel (Rect (10 + l * 42) (140 + d * 42) 40 40) (d * 3 + l) | l <- [0 .. 2]] | d <- [0 .. 3]]
    , _getAmountLabel = (Label (Rect 10 230 100 30) "GetAmount")
    , _dispenseLabel = (Label (Rect 130 230 100 30) "Dispense 100")
    , _ejectLabel = (Label (Rect 10 330 100 30) "Eject")
    }

Lens.Micro.TH.makeLenses ''Rect
Lens.Micro.TH.makeLenses ''Label
Lens.Micro.TH.makeLenses ''InternalState
