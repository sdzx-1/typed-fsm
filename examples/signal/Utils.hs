{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Data.IORef
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Kind
import Data.Void
import Linear
import SDL
import SDL.Font (Font)
import qualified SDL.Font as Font
import TypedFsm.Core
import TypedFsm.Driver
import TypedFsm.Signal

data Button

instance SignalAndSlot Button where
  type Signal Button = ()
  type Slot Button = Void

data Label

instance SignalAndSlot Label where
  type Signal Label = Void
  type Slot Label = String

data MySt

instance SignalAndSlot MySt where
  type Signal MySt = String
  type Slot MySt = ()

type Point' = Point V2 Int

pattern Point x y = P (V2 x y)
{-# COMPLETE Point #-}

newtype MySDLEvent = MyMouseLeftButtonClick (Point V2 Int)

data MyEvent
  = SDLEvent MySDLEvent
  | RecvVal Dynamic

data GlobalSignalEnv = GlobalSignalEnv
  { _sendSignals :: IORef (IntMap [Dynamic])
  , _sig2Slots :: IORef (IntMap IntSet)
  }

data Rect = Rect
  { _rx :: Int
  , _ry :: Int
  , _width :: Int
  , _height :: Int
  }
  deriving (Show)

type CharCache = HashMap Char Texture
type CharCacheRef = IORef CharCache

data DrawEnv = DrawEnv
  { _renderer :: Renderer
  , _font :: Font
  , _charCacheRef :: CharCacheRef
  }

eventToKP :: Event -> Maybe MySDLEvent
eventToKP e = case eventPayload e of
  MouseButtonEvent (MouseButtonEventData _ Pressed _ _ _ pos) ->
    Just $ MyMouseLeftButtonClick (fmap fromIntegral pos)
  _ -> Nothing

makeMyEvent' :: [Event] -> [MySDLEvent]
makeMyEvent' events =
  let fun b a =
        case eventToKP a of
          Nothing -> b
          Just a' -> a' : b
   in foldl' fun [] events

pattern KBE
  :: InputMotion
  -> Keycode
  -> Bool
  -> Maybe Window
  -> Scancode
  -> KeyModifier
  -> EventPayload
pattern KBE press keycode repeat a d f =
  KeyboardEvent
    (KeyboardEventData a press repeat (Keysym d keycode f))

initCharCache :: IO CharCacheRef
initCharCache = newIORef H.empty

getCharTexure :: Renderer -> Font -> CharCacheRef -> Char -> IO Texture
getCharTexure r font cref c = do
  cc <- readIORef cref
  case H.lookup c cc of
    Nothing -> do
      surf <- Font.blendedGlyph font (V4 0 255 0 255) c
      text <- createTextureFromSurface r surf
      freeSurface surf
      modifyIORef' cref (H.insert c text)
      pure text
    Just t -> pure t

drawString :: DrawEnv -> String -> (Int, Int) -> IO ()
drawString (DrawEnv r font cref) st (x', y') = do
  let go _ _ [] = pure ()
      go x y (t : ts) = do
        TextureInfo _ _ w h <- queryTexture t
        copy r t Nothing (Just (Rectangle (P (V2 x y)) (V2 w h)))
        go (x + w) y ts
  txts <- mapM (getCharTexure r font cref) st
  go (fromIntegral x') (fromIntegral y') txts

drawStrings :: DrawEnv -> [String] -> (Int, Int) -> IO ()
drawStrings de sts (x, y) = do
  forM_ (zip [0 ..] sts) $ \(i, st) -> do
    drawString de st (x, y + i * 20)

contains :: Rect -> Point' -> Bool
contains (Rect rx ry w h) (Point x y) =
  (rx <= x && x <= rx + w) && (ry <= y && y <= ry + h)

drawRect' :: DrawEnv -> Rect -> IO ()
drawRect' de@(DrawEnv{_renderer}) (Rect x y w h) = do
  drawRect
    _renderer
    ( Just
        ( Rectangle
            (P (V2 (fromIntegral x) (fromIntegral y)))
            (V2 (fromIntegral w) (fromIntegral h))
        )
    )
