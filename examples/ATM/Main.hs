{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use print" #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Data (Proxy (..))
import Data.IFunctor (IMonad (..))
import qualified Data.IFunctor as I
import Data.Kind
import Data.SR
import EventToMsg
import GHC.TypeError (TypeError)
import GHC.TypeLits (ErrorMessage (..))
import Handler
import Type
import TypedFsm.Core
import TypedFsm.Driver

main :: IO ()
main = do
  putStrLn "start ATM"
  void $ runStateT (evenLoop [] (SomeOperate readyHandler)) (InternalState 1234 1000)

evenLoop :: [String] -> SomeOp ATMSt InternalState -> StateT InternalState IO ()
evenLoop events (SomeOperate fun) = do
  v <- runOp atmDepMap events fun
  case v of
    Left fun1 -> do
      let atmSt = reifySomeOperateInput fun1
      liftIO $ putStrLn ("-> current ATMSt: " <> show atmSt)
      st <- liftIO getLine
      evenLoop [st] fun1
    Right _ -> pure ()
