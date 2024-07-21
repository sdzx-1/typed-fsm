{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module EventToMsg where

import Data.Data (Proxy (..))
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.Singletons (SingI)
import Lens.Micro
import Type
import TypedFsm
import Utils

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
            | otherwise ->
                let nls = ist ^. nlabels
                    rs = filter (\(NLabel r _) -> r `contains` p) nls
                 in case rs of
                      [] -> Nothing
                      NLabel _r i : _ -> case i of
                        10 -> Just $ SomeMsg DelectNum
                        11 -> Just $ SomeMsg (CheckPIN $ reverse (ist ^. tmpPin))
                        _ -> Just $ SomeMsg $ AddNum i
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
