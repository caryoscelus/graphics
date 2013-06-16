{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Graphics.Sprite (Sprite (..) , modulatedSprite) where

-- TODO control export better

import Control.Applicative
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import Data.Word
import Graphics.Rendering.OpenGL.Raw.Core32
import Linear.V2

import Game.Graphics.Texture

-- TODO Make a Polygon type, and make sprites just be a special case
-- of it.

data Sprite =
  Sprite { spriteTexId     :: !GLuint
         , spriteTop       :: !GLfloat
         , spriteRight     :: !GLfloat
         , spriteBottom    :: !GLfloat
         , spriteLeft      :: !GLfloat
         , spriteModulateR :: !GLfloat
         , spriteModulateG :: !GLfloat
         , spriteModulateB :: !GLfloat
         , spriteModulateA :: !GLfloat
         }

-- | @modulatedSprite color topLeft dim tex@ creates a sprite from a texture
modulatedSprite :: Real a => AlphaColour a -> V2 Word -> V2 Word -> Texture -> Sprite
modulatedSprite (alphaColourConvert -> color) (V2 x y) (V2 w h) tex =
  Sprite (texId tex)
  (coord y texH) (coord (x + w - 1) texW) (coord (y + h - 1) texH) (coord x texW) red green blue $ alphaChannel color
  where coord a b = fromIntegral a / b
        V2 texW texH = fromIntegral <$> texSize tex
        RGB red green blue = toRGB $ color `over` black
