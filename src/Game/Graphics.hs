{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Game.Graphics
       ( Space ()
       , transform, translate, rotate, scale, shear, reflect
       , Sampling (..), Alpha (..), Texture ()
       , texture, loadTexture
       , Sprite (), sprite, modulatedSprite
       , GraphicsState (), Triangles.initializeGraphics, draw, clear
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

type Sprite = Triangles.Triangles

draw :: GraphicsState -> Space Sprite -> IO Bool
{-# INLINE draw #-}
draw gs = Triangles.draw gs . map (uncurry $ flip Triangles.applyTransform) . runSpace

clear :: IO ()
{-# INLINE clear #-}
clear = glClear gl_COLOR_BUFFER_BIT

modulatedSprite :: Real a => AlphaColour a -> V2 Int -> V2 Int -> Texture -> Space Sprite
{-# INLINE modulatedSprite #-}
modulatedSprite color pos dim tex = return $! Triangles.sprite tex color pos dim

sprite :: V2 Int -> V2 Int -> Texture -> Space Sprite
{-# INLINE sprite #-}
sprite = modulatedSprite (opaque (white :: Colour GLfloat))
