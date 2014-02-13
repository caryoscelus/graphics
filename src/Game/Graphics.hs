{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Game.Graphics
       ( Space ()
       , transform, translate, rotate, scale, shear, reflect
       , Sampling (..), Alpha (..), Texture ()
       , texture, loadTexture, freeTexture
       , Sprite (), sprite, modulatedSprite
       , GraphicsState (), Triangles.initializeGraphics
       , Triangles.freeGraphics, draw, clear
       , GLfloat, glViewport
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer.Stricter
import Data.Colour
import Data.Colour.Names                    (white)
import Data.Foldable
import Data.Traversable
import Game.Graphics.AffineTransform        (AffineTransform)
import Game.Graphics.Texture
import Game.Graphics.Triangles              (GraphicsState)
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

import qualified Game.Graphics.AffineTransform as Transform
import qualified Game.Graphics.Triangles       as Triangles
import qualified Game.Graphics.Font            as Font

newtype Space a = Space { unSpace :: WriterT AffineTransform [] a }
                deriving ( Functor, Applicative, Alternative, Monad
                         , MonadPlus, MonadFix, Foldable, Traversable
                         )

runSpace :: Space a -> [(a, AffineTransform)]
{-# INLINE runSpace #-}
runSpace = runWriterT . unSpace

transform :: AffineTransform -> Space ()
{-# INLINE transform #-}
transform = Space . tell

translate :: V2 GLfloat -> Space ()
{-# INLINE translate #-}
translate = transform . Transform.translate

rotate :: GLfloat -> Space ()
{-# INLINE rotate #-}
rotate = transform . Transform.rotate

scale :: V2 GLfloat -> Space ()
{-# INLINE scale #-}
scale = transform . Transform.scale

shear :: V2 GLfloat -> Space ()
{-# INLINE shear #-}
shear = transform . Transform.shear

reflect :: V2 GLfloat -> Space ()
{-# INLINE reflect #-}
reflect = transform . Transform.reflect

data Sprite = SpriteTriangle Triangles.Triangles | SpriteFont Font.FontText

isTriangle :: Sprite -> Bool
isTriangle (SpriteTriangle _) = True
isTriangle _ = False

isFont :: Sprite -> Bool
isFont (SpriteFont _) = True
isFont _ = False

getTriangle :: Sprite -> Triangles.Triangles
getTriangle (SpriteTriangle x) = x
getTriangle _ = error "Not a triangle sprite"

-- | This function will make the following changes to OpenGL state by
-- the time it returns:
--
--   * the current vertex array binding will be unset
--   * the current program will be unset
--   * the active texture unit will be set to GL_TEXTURE0
--   * GL_TEXTURE_2D will be unbound
--   * GL_ARRAY_BUFFER and GL_ELEMENT_ARRAY_BUFFER will be unbound
--   * the blend function will be set to GL_ONE, GL_ONE_MINUS_SRC_ALPHA
--   * blending will be enabled
--   * GL_FRAMEBUFFER_SRGB will be enabled on the current framebuffer
draw :: GraphicsState -> Space Sprite -> IO Bool
{-# INLINE draw #-}
draw gs sp = Triangles.draw gs triangles
    where
        triangles = map ttran . filter (isTriangle . fst) $ sprites
        sprites = runSpace $ sp
        ttran (spr, trans) = Triangles.applyTransform trans $ getTriangle spr

clear :: IO ()
{-# INLINE clear #-}
clear = glClear gl_COLOR_BUFFER_BIT

modulatedSprite :: AlphaColour GLfloat -> V2 Int -> V2 Int -> Texture -> Space Sprite
{-# INLINE modulatedSprite #-}
modulatedSprite color pos dim tex = return . SpriteTriangle $! Triangles.sprite tex color pos dim

sprite :: V2 Int -> V2 Int -> Texture -> Space Sprite
{-# INLINE sprite #-}
sprite = modulatedSprite (opaque (white :: Colour GLfloat))
