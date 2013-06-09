{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.Graphics.Sprite (Texture (), Sprite (..), loadTexture, texture, sprite) where

-- TODO control export better

import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Monad
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Game.Graphics.Utils
import Graphics.Rendering.OpenGL.Raw.Core32
import Linear.V2
import qualified Data.Vector.Storable as Vector

data Texture =
  Texture { texId     :: !GLuint
          , texSize   :: !(V2 Word)
          }

-- TODO Make a Polygon type, and make sprites just be a special case
-- of it.

data Sprite =
  Sprite { spriteTexId  :: !GLuint
         , spriteTop    :: !GLfloat
         , spriteRight  :: !GLfloat
         , spriteBottom :: !GLfloat
         , spriteLeft   :: !GLfloat
         }

-- TODO add support for custom mipmaps, or write a high quality
-- mipmapper right here

-- TODO add support for using texture arrays automatically on machines
-- that support them

-- TODO use premultiplied alpha for more better blending

-- | Create a texture from an image loaded using JuicyPixels.
texture :: DynamicImage -> IO Texture
texture dynImg = do
  origTid <- glGet gl_TEXTURE_BINDING_2D
  tid <- glGen glGenTextures
  glBindTexture gl_TEXTURE_2D tid
  (w, h) <- case dynImg of
    ImageY8     img -> texImage2D gl_SRGB8        gl_RED  gl_UNSIGNED_BYTE img
    ImageYF     img -> texImage2D gl_RGB32F       gl_RED  gl_FLOAT         img
    ImageYA8    img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE (promoteImage img :: Image PixelRGBA8)
    ImageRGB8   img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE img
    ImageRGBF   img -> texImage2D gl_RGB32F       gl_RGB  gl_FLOAT         img 
    ImageRGBA8  img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE img
    ImageYCbCr8 img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE (convertImage img :: Image PixelRGB8)
  glGenerateMipmap gl_TEXTURE_2D
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
  glBindTexture gl_TEXTURE_2D origTid
  return $! Texture tid $ V2 w h
  where texImage2D :: Storable (PixelBaseComponent b) => GLenum -> GLenum -> GLenum -> Image b -> IO (Word, Word)
        texImage2D internal format type_ img = do
          let w = imageWidth img
              h = imageHeight img
          unsafeWith img $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral internal)
            (fromIntegral w) (fromIntegral h) 0 format type_
          return $! (fromIntegral w, fromIntegral h)
        unsafeWith :: Storable (PixelBaseComponent a) => Image a -> (Ptr (PixelBaseComponent a) -> IO b) -> IO b
        unsafeWith = Vector.unsafeWith . imageData

loadTexture :: FilePath -> IO (Either String Texture)
loadTexture = traverseEither texture <=< readImage
  where traverseEither _ (Left l) = return (Left l)
        traverseEither f (Right r) = Right <$> f r

-- TODO It's quite annoying that the newly created sprite does not
-- match the dimensions of the texture selection. Maybe make a
-- convenience function for creating the appropriately adjusted
-- space. This may be easier once we've added a more general polygon
-- generator.

-- | @sprite top right bottom left texture@ creates a sprite from a texture
sprite :: Word -> Word -> Word -> Word -> Texture -> Sprite
sprite t r b l tex = Sprite (texId tex) (coord t h) (coord r w) (coord b h) (coord l w)
  where coord x y = fromIntegral x / y
        V2 w h = fromIntegral <$> texSize tex
