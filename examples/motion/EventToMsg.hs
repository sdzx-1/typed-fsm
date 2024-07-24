{-# LANGUAGE ViewPatterns #-}

module EventToMsg where

import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.Singletons (SingI (..))
import Type
import TypedFsm
import Utils

mouseDepMap :: State2GenMsg Motion MotionState MyEvent
mouseDepMap =
  D.fromList
    [ SIdle
        :=> GenMsg
          ( \(MotionState rect' _ _ _) event -> case event of
              MyQuit -> Just (SomeMsg sing ExitMotion)
              MyMouseMotion (fmap fromIntegral -> p) tms ->
                if rect' `contains` p
                  then Just $ SomeMsg sing (MoveIn p tms)
                  else Nothing
              _ -> Nothing
          )
    , SOver
        :=> GenMsg
          ( \(MotionState rect' _ _ _) event -> case event of
              MyQuit -> Just (SomeMsg sing ExitMotion)
              MyMouseMotion (fmap fromIntegral -> p) tms ->
                if rect' `contains` p
                  then Just $ SomeMsg sing (InMove p tms)
                  else Just $ SomeMsg sing MoveOut
              MyTimeout -> Just $ SomeMsg sing TimeoutH
          )
    , SHover
        :=> GenMsg
          ( \(MotionState rect' _ (Point mx my) _) event -> case event of
              MyQuit -> Just (SomeMsg sing ExitMotion)
              MyMouseMotion (fmap fromIntegral -> p@(Point x y)) tms ->
                if rect' `contains` p
                  then
                    if abs (mx - x) < 30 && abs (my - y) < 30
                      then Nothing
                      else
                        Just $ SomeMsg sing (HInMove p tms)
                  else Just $ SomeMsg sing HMoveOut
              _ -> Nothing
          )
    ]
