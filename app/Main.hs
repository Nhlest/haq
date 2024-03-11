{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Raylib.Core
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Core.Textures
import Raylib.Types.Core
import Raylib.Types.Core.Textures
import Graphics.GL.Compatibility30
import qualified Data.Vector.Storable as SV
import Foreign hiding (void)
import Raylib.Util.RLGL (rlCheckErrors)
import Control.Monad

main :: IO ()
main = void $ withWindow 800 600 "raylib" 60 startup

startup :: WindowResources -> IO WindowResources
startup r = do
  image <- genImageColor 200 200 $ Color 255 0 0 255
  texture <- loadTextureFromImage image r
  let v :: SV.Vector Word8 = SV.replicate (200*200*4) 0
  let v2 :: SV.Vector Word8 = v SV.// concat [[(x*4+3, 255), (x*4+1, 255)]| x <- [0..(200*200-1)]]
  
  mainLoop v2 texture False r

mainLoop :: SV.Vector Word8 -> Texture -> Bool -> WindowResources -> IO WindowResources
mainLoop _ _ True r = pure r
mainLoop v texture _ r = do
  let ptr = fst $ SV.unsafeToForeignPtr0 v
  let t_id = texture'id texture
  glBindTexture GL_TEXTURE_2D (fromInteger t_id)
  withForeignPtr ptr $ \p -> do
    glTexSubImage2D GL_TEXTURE_2D 0 0 0 100 100 GL_RGBA GL_UNSIGNED_BYTE p
  rlCheckErrors
  drawing $ do
    clearBackground lightGray
    drawTexturePro texture (Rectangle 0.0 0.0 200.0 200.0) (Rectangle 0.0 0.0 800.0 600.0) (Vector2 0.0 0.0) 0.0 white
  shouldClose <- windowShouldClose
  mainLoop v texture shouldClose r
