{-# LANGUAGE NoMonomorphismRestriction #-}
module Game.Sprite (Texture (), Sprite (), texture, sprite) where

import Codec.Picture
import Codec.Picture.Types
import qualified Data.Vector.Storable as Vector
import Graphics.Rendering.OpenGL.Raw.Core32
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Storable

data Texture =
  Texture { texId     :: {-# UNPACK #-} !GLuint
          , texWidth  :: {-# UNPACK #-} !Word
          , texHeight :: {-# UNPACK #-} !Word
          }

data Sprite =
  Sprite { spriteTexId  :: {-# UNPACK #-} !GLuint
         , texTop       :: {-# UNPACK #-} !GLfloat
         , texRight     :: {-# UNPACK #-} !GLfloat
         , texBottom    :: {-# UNPACK #-} !GLfloat
         , texLeft      :: {-# UNPACK #-} !GLfloat
         }

-- TODO add support for mipmaps

-- | Create a texture from an image loaded using JuicyPixels.
--
-- OpenGL effects: changes the texture bound to GL_TEXTURE_2D
texture :: DynamicImage -> IO Texture
texture dynImg = do
  tid <- alloca $ \ptr -> glGenTextures 1 ptr >> peek ptr
  glBindTexture gl_TEXTURE_2D tid
  (w, h) <- case dynImg of
    ImageY8     img -> texImage2D gl_SRGB8        gl_RED  gl_UNSIGNED_BYTE img
    ImageYF     img -> texImage2D gl_RGB32F       gl_RED  gl_FLOAT         img
    ImageYA8    img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE (promoteImage img :: Image PixelRGBA8)
    ImageRGB8   img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE img
    ImageRGBF   img -> texImage2D gl_RGB32F       gl_RGB  gl_FLOAT         img 
    ImageRGBA8  img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE img
    ImageYCbCr8 img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE (convertImage img :: Image PixelRGB8)
  return $! Texture tid w h
  where texImage2D internal format type_ img = do
          let w = imageWidth img
              h = imageHeight img
          unsafeWith img $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral internal)
            (fromIntegral w) (fromIntegral h) 0 format type_
          return $! (fromIntegral w, fromIntegral h)
        unsafeWith = Vector.unsafeWith . imageData

-- | @sprite top right bottom left texture@ creates a sprite from a texture
sprite :: Word -> Word -> Word -> Word -> Texture -> Sprite
sprite t r b l tex = Sprite (texId tex) (coord t h) (coord r w) (coord b h) (coord l w)
  where coord x y = fromIntegral x / y
        w = fromIntegral $ texWidth tex
        h = fromIntegral $ texHeight tex
