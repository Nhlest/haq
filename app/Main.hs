{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Raylib.Core
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Core.Textures
import Raylib.Types.Core
import Raylib.Types.Core.Textures
import Graphics.GL.Compatibility30
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as SV
import Foreign hiding (void)
import Raylib.Util.RLGL (rlCheckErrors)
import Control.Monad

import Lib

main :: IO ()
main = do
  ast <- parseHaQF "hello.haq"
  print ast
  -- setConfigFlags [VsyncHint]
  -- setTargetFPS 0
  -- void $ withWindow 800 600 "raylib" 60 startup

startup :: WindowResources -> IO WindowResources
startup r = do
  setTargetFPS 0

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  image <- genImageColor 200 200 $ Color 255 0 0 255
  n_image <- imageFormat image PixelFormatUncompressedR5G5B5A1
  texture <- loadTextureFromImage n_image r
  let v :: SV.Vector Word8 = SV.replicate (200*200*2) 0

  mv <- SV.thaw v
  
  mainLoop 0 mv texture False r

changeColor :: Ptr Word8 -> Int -> (Int, Int, Int, Int) -> IO ()
changeColor ptr i (r, g, b, a) = do
    let p1 :: Ptr Word8 = plusPtr ptr (i*2)
    let p2 :: Ptr Word8 = plusPtr ptr (i*2 + 1)
    let c = a * 32768 + b * 1024 + g * 32 + r
    let (b1, b2) = divMod c 256 
    -- poke p1 (fromIntegral b1)
    -- poke p2 (fromIntegral b2)
    poke p1 31
    poke p2 128

mainLoop :: Int -> MV.IOVector Word8 -> Texture -> Bool -> WindowResources -> IO WindowResources
mainLoop _ _ _ True r = pure r
mainLoop ii v texture _ r = do
  MV.unsafeWith v $ \ptr -> do
    let t_id = texture'id texture
    glBindTexture GL_TEXTURE_2D (fromInteger t_id)
    glTexSubImage2D GL_TEXTURE_2D 0 0 0 200 200 GL_RGBA GL_UNSIGNED_BYTE ptr
    rlCheckErrors
    drawing $ do
      clearBackground lightGray
      drawTexturePro texture (Rectangle 0.0 0.0 200.0 200.0) (Rectangle 0.0 0.0 800.0 600.0) (Vector2 0.0 0.0) 0.0 white
    changeColor ptr ii (31, 0, 0, 31)
  shouldClose <- windowShouldClose
  mainLoop (ii+1) v texture shouldClose r
