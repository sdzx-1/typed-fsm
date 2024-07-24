{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module EventToMsg where

import Data.Data (Proxy (..))
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.Singletons (SingI (..))
import Lens.Micro
import Type
import TypedFsm
import Utils

mkSomeMsg
  :: forall ps (from :: ps) (to :: ps)
   . (SingI to) => Msg ps from to -> Maybe (SomeMsg ps from)
mkSomeMsg msg = Just (SomeMsg (sing @to) msg)

scEventHandler
  :: (SingI n, Less3 n)
  => Proxy n
  -> GenMsg ATMSt InternalState MyEvent (CardInserted n)
scEventHandler _ =
  GenMsg
    ( \ist event -> case event of
        MyMouseLeftButtonClick (fmap fromIntegral -> p) ->
          if
            | (ist ^. ejectLabel . rect) `contains` p -> mkSomeMsg CIEject
            | otherwise ->
                let nls = ist ^. nlabels
                    rs = filter (\(NLabel r _) -> r `contains` p) nls
                 in case rs of
                      [] -> Nothing
                      NLabel _r i : _ -> case i of
                        10 -> mkSomeMsg DelectNum
                        11 -> mkSomeMsg (CheckPIN $ reverse (ist ^. tmpPin))
                        _ -> mkSomeMsg $ AddNum i
    )

atmDepMap :: State2GenMsg ATMSt InternalState MyEvent
atmDepMap =
  D.fromList
    [ SReady
        :=> GenMsg
          ( \ist event -> case event of
              MyMouseLeftButtonClick (fmap fromIntegral -> p) ->
                ( if
                    | (ist ^. insCardLabel . rect) `contains` p -> mkSomeMsg InsertCard
                    | (ist ^. exitLabel . rect) `contains` p -> mkSomeMsg ExitATM
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
                  | (ist ^. ejectLabel . rect) `contains` p -> mkSomeMsg SEject
                  | (ist ^. getAmountLabel . rect) `contains` p -> mkSomeMsg GetAmount
                  | (ist ^. dispenseLabel . rect) `contains` p -> mkSomeMsg (Dispense 100)
                  | otherwise -> Nothing
          )
    ]
