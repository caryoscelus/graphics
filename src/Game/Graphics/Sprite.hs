{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Graphics.Sprite
       ( Texture (), Sprite (..), Sampling (..)
       , loadTexture, texture, texturePremultiplied, modulatedSprite
       ) where

-- TODO control export better

import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Monad
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Game.Graphics.Utils
import Graphics.Rendering.OpenGL.Raw.Core32
import Linear.V2
import Linear.V4

import qualified Data.Vector.Storable as Vector

data Texture =
  Texture { texId     :: !GLuint
          , texSize   :: !(V2 Word)
          }

-- TODO Make a Polygon type, and make sprites just be a special case
-- of it.

data Sprite =
  Sprite { spriteTexId         :: !GLuint
         , spriteTop           :: !GLfloat
         , spriteRight         :: !GLfloat
         , spriteBottom        :: !GLfloat
         , spriteLeft          :: !GLfloat
         , spriteModulateColor :: !(V4 GLfloat)
         }

-- TODO support for tiling textures, somehow

-- TODO add support for custom mipmaps, or write a high quality
-- mipmapper right here

-- TODO add support for using texture arrays automatically on machines
-- that support them

premultiplyAlpha :: Image PixelRGBA8 -> Image PixelRGBA8
premultiplyAlpha = pixelMap $ fromColour . toColour
  where toColour (PixelRGBA8 r g b a) =
          sRGB24 r g b `withOpacity`
          (fromIntegral a / fromIntegral (maxBound :: Word8)) :: AlphaColour Double
        fromColour alphaColor =
          let a = round $ alphaChannel alphaColor * fromIntegral (maxBound :: Word8)
              RGB r g b = toSRGB24 $ alphaColor `over` black
          in PixelRGBA8 r g b a

data Sampling = Nearest | Linear deriving (Eq, Ord, Read, Show)

setSampling :: Sampling -> IO ()
setSampling Nearest = do
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
setSampling Linear = do
  glGenerateMipmap gl_TEXTURE_2D
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR

-- | Create a texture from an image loaded using JuicyPixels.
texture' :: (Image PixelRGBA8 -> Image PixelRGBA8) -> Sampling -> DynamicImage -> IO Texture
texture' preprocess sampling dynImg = do
  origTid <- glGet gl_TEXTURE_BINDING_2D
  tid <- glGen glGenTextures
  glBindTexture gl_TEXTURE_2D tid
  (w, h) <- case dynImg of
    ImageY8     img -> texImage2D gl_SRGB8        gl_RED  gl_UNSIGNED_BYTE img
    ImageYF     img -> texImage2D gl_RGB32F       gl_RED  gl_FLOAT         img
    ImageYA8    img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE $ preprocess (promoteImage img :: Image PixelRGBA8)
    ImageRGB8   img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE img
    ImageRGBF   img -> texImage2D gl_RGB32F       gl_RGB  gl_FLOAT         img
    ImageRGBA8  img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE $ preprocess img
    ImageYCbCr8 img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE (convertImage img :: Image PixelRGB8)
  setSampling sampling
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

texture :: Sampling -> DynamicImage -> IO Texture
texture = texture' premultiplyAlpha

texturePremultiplied :: Sampling -> DynamicImage -> IO Texture
texturePremultiplied = texture' id

loadTexture :: Sampling -> FilePath -> IO (Either String Texture)
loadTexture sampling = traverseEither (texture sampling) <=< readImage
  where traverseEither _ (Left l) = return (Left l)
        traverseEither f (Right r) = Right <$> f r

-- | @modulatedSprite color topLeft dim tex@ creates a sprite from a texture
modulatedSprite :: Real a => AlphaColour a -> V2 Word -> V2 Word -> Texture -> Sprite
modulatedSprite (alphaColourConvert -> color) (V2 x y) (V2 w h) tex =
  Sprite (texId tex)
  (coord y texH) (coord (x + w - 1) texW) (coord (y + h - 1) texH) (coord x texW) (V4 red green blue $ alphaChannel color)
  where coord a b = fromIntegral a / b
        V2 texW texH = fromIntegral <$> texSize tex
        RGB red green blue = toRGB $ color `over` black
