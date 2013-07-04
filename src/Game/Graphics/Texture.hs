{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
module Game.Graphics.Texture
       ( Texture (), Sampling (..), TextureError (..)
       , loadTexture, loadTexturePremultiplied
       , texture, texturePremultiplied
       , texId, texSize
       ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Game.Graphics.Utils
import Graphics.Rendering.OpenGL.Raw.Core32
import Linear.V2

import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Unboxed  as Unboxed

data Texture =
  Texture { texId   :: !GLuint
          , texSize :: !(V2 Word)
          }

-- TODO support for tiling textures, somehow

-- TODO Add support for custom mipmaps, or write a high quality
-- mipmapper right here. The main point is that I just don't trust all
-- OpenGL drivers to do it right.

-- TODO add support for using texture arrays automatically on machines
-- that support them

-- TODO Add support for texture arrays, under the hood, when
-- available. This should be a pretty big speedup for certain kinds of
-- graphics engines (e.g. any game where sprites with different
-- textures can be dynamically reordered front to back).

premultiplyAlpha :: Image PixelRGBA8 -> Image PixelRGBA8
premultiplyAlpha = pixelMap (\(PixelRGBA8 r g b a) -> PixelRGBA8 (f a r) (f a g) (f a b) a)
  where premultiplyChannel :: Word8 -> Word8 -> Word8
        premultiplyChannel a =
          round . (* m) . toSRGB . (* (fromIntegral a / m)) . fromSRGB . (/ m) .
          fromIntegral
        toSRGB, fromSRGB :: Double -> Double
        toSRGB lin | lin == 1         = 1
                   | lin <= 0.0031308 = 12.92*lin
                   | otherwise        = (1 + transA)*lin**(1/2.4) - transA
        fromSRGB nonLin | nonLin == 1       = 1
                        | nonLin <= 0.04045 = nonLin/12.92
                        | otherwise         = ((nonLin + transA)/(1 + transA))**2.4
        transA = 0.055
        lut :: Unboxed.Vector Word8
        {-# NOINLINE lut #-}
        lut = Unboxed.concatMap
              (\a -> Unboxed.generate (m+1) $
               premultiplyChannel a . fromIntegral) $ Unboxed.enumFromN 0 (m+1)
        m :: Num a => a
        m = fromIntegral (maxBound :: Word8)
        f a c = lut Unboxed.! (fromIntegral a * m + fromIntegral c)

data Sampling = Nearest | Linear deriving (Eq, Ord, Read, Show)

setSampling :: Sampling -> IO ()
setSampling Nearest = do
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
setSampling Linear = do
  glGenerateMipmap gl_TEXTURE_2D
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR

data TextureError =
  DimensionsTooLarge { driverLimit  :: !Word
                     , actualWidth  :: !Word
                     , actualHeight :: !Word
                     } deriving (Show, Typeable)

instance Exception TextureError

-- | Create a texture from an image loaded using JuicyPixels.
texture' :: (Image PixelRGBA8 -> Image PixelRGBA8) -> Sampling -> DynamicImage -> IO Texture
texture' preprocess sampling dynImg =
  case dynImg of
    ImageY8     img -> texImage2D gl_SRGB8        gl_RED  gl_UNSIGNED_BYTE img
    ImageYF     img -> texImage2D gl_RGB32F       gl_RED  gl_FLOAT         img
    ImageYA8    img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE $ preprocess (promoteImage img :: Image PixelRGBA8)
    ImageRGB8   img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE img
    ImageRGBF   img -> texImage2D gl_RGB32F       gl_RGB  gl_FLOAT         img
    ImageRGBA8  img -> texImage2D gl_SRGB8_ALPHA8 gl_RGBA gl_UNSIGNED_BYTE $ preprocess img
    ImageYCbCr8 img -> texImage2D gl_SRGB8        gl_RGB  gl_UNSIGNED_BYTE (convertImage img :: Image PixelRGB8)
  where texImage2D :: Storable (PixelBaseComponent b) => GLenum -> GLenum -> GLenum -> Image b -> IO Texture
        texImage2D internal format type_ img = do
          let w, h :: Num a => a
              w = fromIntegral $ imageWidth img
              h = fromIntegral $ imageHeight img :: Num a => a
          maxDim <- glGet gl_MAX_TEXTURE_SIZE
          when (maxDim < w || maxDim < h) . throwIO $ DimensionsTooLarge maxDim w h
          origTid <- glGet gl_TEXTURE_BINDING_2D
          tid <- glGen glGenTextures
          glBindTexture gl_TEXTURE_2D tid
          unsafeWith img $ glTexImage2D gl_TEXTURE_2D 0 (fromIntegral internal) w h 0 format type_
          setSampling sampling
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
          glBindTexture gl_TEXTURE_2D origTid
          return $! Texture tid $ V2 w h
        unsafeWith :: Storable (PixelBaseComponent a) => Image a -> (Ptr (PixelBaseComponent a) -> IO b) -> IO b
        unsafeWith = Vector.unsafeWith . imageData

texture :: Sampling -> DynamicImage -> IO Texture
texture = texture' premultiplyAlpha

texturePremultiplied :: Sampling -> DynamicImage -> IO Texture
texturePremultiplied = texture' id

loadTexture' :: (Sampling -> DynamicImage -> IO Texture) -> Sampling -> FilePath -> IO (Either String Texture)
loadTexture' f sampling = traverseEither (f sampling) <=< readImage
  where traverseEither _ (Left l) = return (Left l)
        traverseEither g (Right r) = Right <$> g r

loadTexture :: Sampling -> FilePath -> IO (Either String Texture)
loadTexture = loadTexture' texture

loadTexturePremultiplied :: Sampling -> FilePath -> IO (Either String Texture)
loadTexturePremultiplied = loadTexture' texturePremultiplied
