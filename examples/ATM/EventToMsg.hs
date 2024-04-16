{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EventToMsg where

import Data.Data (Proxy (..))
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.SR
import Text.Read (readMaybe)
import Type
import TypedFsm.Driver

type MyEvent = String

scEventHandler
  :: (SingI n, Reify n, Less3 n)
  => Proxy n
  -> GenMsg ATMSt InternalState MyEvent (CardInserted n)
scEventHandler _ =
  GenMsg
    ( \_ event -> case words event of
        "eject" : _ -> Just (SomeMsg CIEject)
        "checkPin" : mNumString : _ ->
          case readMaybe @Int mNumString of
            Just v -> Just (SomeMsg $ CheckPIN v)
            Nothing -> Nothing
        _ -> Nothing
    )

atmDepMap :: State2GenMsg ATMSt InternalState MyEvent
atmDepMap =
  D.fromList
    [ SReady
        :=> GenMsg
          ( \_ event -> case event of
              "ins card" -> Just (SomeMsg InsertCard)
              _ -> Nothing
          )
    , SCardInserted SZ :=> scEventHandler Proxy
    , SCardInserted (SS SZ) :=> scEventHandler Proxy
    , SCardInserted (SS (SS SZ)) :=> scEventHandler Proxy
    , SSession
        :=> GenMsg
          ( \_ event -> case words event of
              "eject" : _ -> Just (SomeMsg SEject)
              "getAmount" : _ -> Just (SomeMsg GetAmount)
              "dispense" : mNumString : _ ->
                case readMaybe @Int mNumString of
                  Just v -> Just (SomeMsg $ Dispense v)
                  Nothing -> Nothing
              _ -> Nothing
          )
    ]
