{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}

module Utils where

import Control.Monad
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Int (Int32)
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
  MouseMotionEvent (MouseMotionEventData _ _ _ pos _) ->
    Just $ MyMouseMotion pos (eventTimestamp e)
  _ -> Nothing

makeMyEvent :: Maybe () -> [Event] -> [MyEvent]
makeMyEvent ma events =
  let fun b a =
        case eventToKP a of
          Nothing -> b
          Just a' -> a' : b
      res = reverse (foldl' fun [] events)
   in case ma of
        Nothing -> res
        Just _ -> MyTimeout : res

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
