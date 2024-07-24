{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Control.Monad (void)
import Control.Monad.State
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.GADT.Compare (GCompare (..), GEq (..))
import Data.IFunctor as I
import Data.Singletons.Base.TH
import Data.Type.Equality (TestEquality (testEquality))
import TypedFsm

$(singletons [d|data TurnSt = Locked | Unlocked | Exit deriving (Show, Eq, Ord)|])

instance StateTransMsg TurnSt where
  data Msg TurnSt from to where
    Coin :: Msg TurnSt Locked Unlocked
    Push :: Msg TurnSt Unlocked Locked
    ExitTurnSt :: Msg TurnSt Locked Exit

instance GEq STurnSt where
  geq = testEquality

instance GCompare STurnSt where
  gcompare = sOrdToGCompare

lockedHandler :: Op TurnSt Int IO () Exit Locked
lockedHandler = I.do
  msg <- getInput
  case msg of
    Coin -> I.do
      liftm $ do
        liftIO $ putStrLn "Coin, internal int add one"
        modify' (+ 1)
        v <- get
        liftIO $ putStrLn $ "Now coin: " ++ show v
      unlockedHandler
    ExitTurnSt -> I.do
      liftm $ liftIO $ putStrLn "Exit turnSt"

unlockedHandler :: Op TurnSt Int IO () Exit Unlocked
unlockedHandler = I.do
  msg <- getInput
  case msg of
    Push -> I.do
      liftm $ do
        liftIO $ putStrLn "Push"
      lockedHandler

eventDepMap :: State2GenMsg TurnSt Int String
eventDepMap =
  D.fromList
    [ SLocked
        :=> GenMsg
          ( \_ ev -> case ev of
              "coin" -> Just $ SomeMsg sing Coin
              "exit" -> Just $ SomeMsg sing ExitTurnSt
              _ -> Nothing
          )
    , SUnlocked
        :=> GenMsg
          ( \_ ev -> case ev of
              "push" -> Just $ SomeMsg sing Push
              _ -> Nothing
          )
    ]

loop
  :: Result TurnSt (NotFoundGenMsg TurnSt) (StateT Int IO) ()
  -> StateT Int IO ()
loop res = do
  case res of
    Finish _ -> liftIO $ putStrLn "Finish"
    ErrorInfo (NotFoundGenMsg (SomeSing singst)) -> liftIO $ putStrLn $ "NotFound GenMSg: " ++ show singst
    Cont sop@(SomeOperate sinput op) -> do
      st <- liftIO $ do
        putStrLn $ "current state: " ++ show (getSomeOperateSt sop)
        putStrLn "input command:"
        getLine
      nres <- runOp eventDepMap [st] sinput op
      loop nres

main :: IO ()
main = do
  putStrLn "start loop"
  void $ runStateT (loop (Cont $ SomeOperate sing lockedHandler)) 0