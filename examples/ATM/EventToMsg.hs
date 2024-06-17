{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module EventToMsg where

import Data.Data (Proxy (..))
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Lens.Micro
import Text.Read (readMaybe)
import Type
import TypedFsm.Driver
import Utils
import Data.Singletons (SingI)

scEventHandler
  :: (SingI n, Less3 n)
  => Proxy n
  -> GenMsg ATMSt InternalState MyEvent (CardInserted n)
scEventHandler _ =
  GenMsg
    ( \ist event -> case event of
        MyMouseLeftButtonClick (fmap fromIntegral -> p) ->
          if
            | (ist ^. ejectLabel . rect) `contains` p -> Just (SomeMsg CIEject)
            | (ist ^. checkPinLabel . rect) `contains` p -> Just (SomeMsg (CheckPIN 1234))
            | (ist ^. checkPinErrorLabel . rect) `contains` p -> Just (SomeMsg (CheckPIN 1))
            | otherwise -> Nothing
    )

atmDepMap :: State2GenMsg ATMSt InternalState MyEvent
atmDepMap =
  D.fromList
    [ SReady
        :=> GenMsg
          ( \ist event -> case event of
              MyMouseLeftButtonClick (fmap fromIntegral -> p) ->
                ( if
                    | (ist ^. insCardLabel . rect) `contains` p -> Just (SomeMsg InsertCard)
                    | (ist ^. exitLabel . rect) `contains` p -> Just (SomeMsg ExitATM)
                    | otherwise -> Nothing
                )
          )
    , SCardInserted SZ :=> scEventHandler Proxy
    , SCardInserted (SS SZ) :=> scEventHandler Proxy
    , SCardInserted (SS (SS SZ)) :=> scEventHandler Proxy
    , SSession
        :=> GenMsg
          ( \ist event -> case event of
              MyMouseLeftButtonClick (fmap fromIntegral -> p) ->
                if
                  | (ist ^. ejectLabel . rect) `contains` p -> Just (SomeMsg SEject)
                  | (ist ^. getAmountLabel . rect) `contains` p -> Just (SomeMsg GetAmount)
                  | (ist ^. dispenseLabel . rect) `contains` p -> Just (SomeMsg (Dispense 100))
                  | otherwise -> Nothing
          )
    ]
