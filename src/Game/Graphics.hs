{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Graphics
       ( Space ()
       , mapTransform
       , transform, translate, rotate, scale, shear, reflect
       , Sampling (..), Texture ()
       , texture, texturePremultiplied, loadTexture, loadTexturePremultiplied
       , Sprite (), sprite, modulatedSprite
       , GraphicsState (), Triangles.initializeGraphics, draw, clear
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer.Stricter
import Data.Colour
import Data.Colour.Names (white)
import Data.Foldable
import Data.Traversable
import Game.Graphics.AffineTransform (AffineTransform)
import Game.Graphics.Triangles (GraphicsState)
import Game.Graphics.Texture
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

import qualified Game.Graphics.AffineTransform as Transform
import qualified Game.Graphics.Triangles as Triangles

newtype Space c a = Space { unSpace :: WriterT (AffineTransform c) [] a }
                  deriving ( Functor, Applicative, Alternative, Monad
                           , MonadPlus, MonadFix, Foldable, Traversable
                           )

space :: Num c => [(a, AffineTransform c)] -> Space c a
{-# INLINE space #-}
space = Space . writerT

runSpace :: Num c => Space c a -> [(a, AffineTransform c)]
{-# INLINE runSpace #-}
runSpace = runWriterT . unSpace

-- TODO put these into a MonadSpace class?

transform :: Num c => AffineTransform c -> Space c ()
{-# INLINE transform #-}
transform = Space . tell

translate :: Num c => V2 c -> Space c ()
{-# INLINE translate #-}
translate = transform . Transform.translate

rotate :: Floating c => c -> Space c ()
{-# INLINE rotate #-}
rotate = transform . Transform.rotate

scale :: Num c => V2 c -> Space c ()
{-# INLINE scale #-}
scale = transform . Transform.scale

shear :: Num c => V2 c -> Space c ()
{-# INLINE shear #-}
shear = transform . Transform.shear

reflect :: Num c => V2 c -> Space c ()
{-# INLINE reflect #-}
reflect = transform . Transform.reflect

type Sprite = Triangles.Triangles

draw :: GraphicsState -> Space GLfloat Sprite -> IO Bool
{-# INLINE draw #-}
draw gs = Triangles.draw gs . map (uncurry $ flip Triangles.applyTransform) . runSpace

clear :: IO ()
{-# INLINE clear #-}
clear = glClear gl_COLOR_BUFFER_BIT

modulatedSprite :: Real a => AlphaColour a -> V2 Int -> V2 Int -> Texture -> Space Int Sprite
{-# INLINE modulatedSprite #-}
modulatedSprite color pos dim tex = return $! Triangles.sprite tex color pos dim

sprite :: V2 Int -> V2 Int -> Texture -> Space Int Sprite
{-# INLINE sprite #-}
sprite = modulatedSprite (opaque (white :: Colour GLfloat))

mapTransform :: (Num t, Num u) => (t -> u) -> Space t a -> Space u a
{-# INLINE mapTransform #-}
mapTransform f = space . (map.second.fmap) f . runSpace
