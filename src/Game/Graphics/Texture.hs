{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.Graphics.Texture
       ( Texture (), Sampling (..), Alpha (..), TextureError (..)
       , loadTexture , texture
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

data Texture =
  Texture { texId   :: !GLuint
          , texSize :: !(V2 Word)
          }

-- TODO Add support for custom mipmaps, or write a high quality
-- mipmapper right here. The main point is that I just don't trust all
-- OpenGL drivers to do it right.

toSRGB :: Double -> Double
toSRGB lin | lin == 1         = 1
           | lin <= 0.0031308 = 12.92*lin
           | otherwise        = (1 + transA)*lin**(1/2.4) - transA
  where transA = 0.055

fromSRGB :: Double -> Double
fromSRGB nonLin | nonLin == 1       = 1
                | nonLin <= 0.04045 = nonLin/12.92
                | otherwise         = ((nonLin + transA)/(1 + transA))**2.4
  where transA = 0.055

asDouble :: (Bounded a, Integral a) => (Double -> Double) -> a -> a
asDouble = (fromDouble .) . (. toDouble)

toDouble :: forall a. (Bounded a, Integral a) => a -> Double
toDouble = (/ fromIntegral (maxBound :: a)) . fromIntegral

fromDouble :: forall a. (Bounded a, Integral a) => Double -> a
fromDouble = round . (* fromIntegral (maxBound :: a))

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

data Alpha = Standard | Premultiplied deriving (Eq, Ord, Read, Show)

-- | Create a texture from an image loaded using JuicyPixels. The
-- internal format is chosen based on the input format. The color
-- spaces for 8 and 16 bit RGB channels are assumed to be sRGB; all
-- others are assumed to be in linear color space.
texture :: Alpha -> Sampling -> DynamicImage -> IO Texture
texture alpha sampling dynImg =
  case dynImg of
    ImageY8     img -> texImage2D sampling gl_SRGB8         gl_RED  gl_UNSIGNED_BYTE  img
    ImageY16    img -> texImage2D sampling gl_RGB16         gl_RED  gl_UNSIGNED_SHORT $
                       (pixelMap.asDouble) fromSRGB img
    ImageYF     img -> texImage2D sampling gl_RGB32F        gl_RED  gl_FLOAT          img
    ImageYA8    img -> texImage2D sampling gl_SRGB8_ALPHA8  gl_RGBA gl_UNSIGNED_BYTE
                       (promoteImage ((whenShouldPremultiply.pixelMap) (\(PixelYA8 y a) ->
                                                  PixelYA8 (fromDouble . toSRGB . (* toDouble a) . fromSRGB . toDouble $ y) a) img) :: Image PixelRGBA8)
    ImageYA16   img -> texImage2D sampling gl_RGBA16  gl_RGBA gl_UNSIGNED_SHORT
                       (promoteImage (pixelMap (\(PixelYA16 y a) ->
                                                 PixelYA16 (fromDouble . whenShouldPremultiply (* toDouble a) . fromSRGB . toDouble $ y) a) img) :: Image PixelRGBA16)
    ImageRGB8   img -> texImage2D sampling gl_SRGB8         gl_RGB  gl_UNSIGNED_BYTE  img
    ImageRGB16  img -> texImage2D sampling gl_RGB16         gl_RGB  gl_UNSIGNED_SHORT $
                       pixelMap (\(PixelRGB16 r g b) ->
                                  let f = asDouble fromSRGB
                                  in PixelRGB16 (f r) (f g) (f b)) img
    ImageRGBF   img -> texImage2D sampling gl_RGB32F        gl_RGB  gl_FLOAT          img
    ImageRGBA8  img -> texImage2D sampling gl_SRGB8_ALPHA8  gl_RGBA gl_UNSIGNED_BYTE $
                       (whenShouldPremultiply.pixelMap) (\(PixelRGBA8 r g b a) ->
                                  let f = fromDouble . toSRGB . (* toDouble a) . fromSRGB . toDouble
                                  in PixelRGBA8 (f r) (f g) (f b) a) img
    ImageRGBA16 img -> texImage2D sampling gl_RGBA16        gl_RGBA gl_UNSIGNED_SHORT $
                       (whenShouldPremultiply.pixelMap) (\(PixelRGBA16 r g b a) ->
                                  let f = fromDouble . toSRGB . (* toDouble a) . fromSRGB . toDouble
                                  in PixelRGBA16 (f r) (f g) (f b) a) img
    ImageYCbCr8 img -> texImage2D sampling gl_SRGB8         gl_RGB  gl_UNSIGNED_BYTE  (convertImage img :: Image PixelRGB8)
    ImageCMYK8  img -> texImage2D sampling gl_RGB8          gl_RGB  gl_UNSIGNED_BYTE  (convertImage img :: Image PixelRGB8)
    ImageCMYK16 img -> texImage2D sampling gl_RGB16         gl_RGB  gl_UNSIGNED_SHORT (convertImage img :: Image PixelRGB16)
  where whenShouldPremultiply f | alpha == Standard = f
                                | otherwise         = id

texImage2D :: Storable (PixelBaseComponent b) => Sampling -> GLenum -> GLenum -> GLenum -> Image b -> IO Texture
texImage2D sampling internal format type_ img = do
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
  where unsafeWith :: Storable (PixelBaseComponent a) => Image a -> (Ptr (PixelBaseComponent a) -> IO b) -> IO b
        unsafeWith = Vector.unsafeWith . imageData

loadTexture' :: (Alpha -> Sampling -> DynamicImage -> IO Texture) -> Alpha -> Sampling -> FilePath -> IO (Either String Texture)
loadTexture' f alpha sampling = traverseEither (f alpha sampling) <=< readImage
  where traverseEither _ (Left l) = return (Left l)
        traverseEither g (Right r) = Right <$> g r

loadTexture :: Alpha -> Sampling -> FilePath -> IO (Either String Texture)
loadTexture = loadTexture' texture
