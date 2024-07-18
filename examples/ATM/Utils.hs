{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}

module Utils where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.IORef
import Linear
import SDL
import SDL.Font (Font)
import qualified SDL.Font as Font
import Type

type CharCache = HashMap Char Texture
type CharCacheRef = IORef CharCache

data DrawEnv = DrawEnv
  { _renderer :: Renderer
  , _font :: Font
  , _charCacheRef :: CharCacheRef
  }

eventToKP :: Event -> Maybe MyEvent
eventToKP e = case eventPayload e of
  MouseButtonEvent (MouseButtonEventData _ Pressed _ _ _ pos) ->
    Just $ MyMouseLeftButtonClick pos
  _ -> Nothing

makeMyEvent :: [Event] -> [MyEvent]
makeMyEvent events =
  let fun b a =
        case eventToKP a of
          Nothing -> b
          Just a' -> a' : b
   in reverse (foldl' fun [] events)

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

drawLabel :: DrawEnv -> Label -> IO ()
drawLabel de@(DrawEnv{_renderer}) (Label (Rect x y w h) st) = do
  drawRect _renderer (Just (Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))))
  drawString de st (x, y)

drawNLabel :: DrawEnv -> NLabel -> IO ()
drawNLabel de@(DrawEnv{_renderer}) (NLabel (Rect x y w h) st) = do
  drawRect _renderer (Just (Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))))
  drawString de (showInt st) (x, y)

showInt :: Int -> String
showInt 10 = "<-"
showInt 11 = "OK"
showInt i = show i

contains :: Rect -> Point' -> Bool
contains (Rect rx1 ry1 w h) (Point x y) =
  (rx1 <= x && x <= rx1 + w) && (ry1 <= y && y <= ry1 + h)
