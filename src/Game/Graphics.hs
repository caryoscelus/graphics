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
space = Space . writerT

runSpace :: Num c => Space c a -> [(a, AffineTransform c)]
runSpace = runWriterT . unSpace

-- TODO put these into a MonadSpace class?

transform :: Num c => AffineTransform c -> Space c ()
transform = Space . tell

translate :: Num c => V2 c -> Space c ()
translate = transform . Transform.translate

rotate :: Floating c => c -> Space c ()
rotate = transform . Transform.rotate

scale :: Num c => V2 c -> Space c ()
scale = transform . Transform.scale

shear :: Num c => V2 c -> Space c ()
shear = transform . Transform.shear

reflect :: Num c => V2 c -> Space c ()
reflect = transform . Transform.reflect

type Sprite = Triangles.Triangles

draw :: GraphicsState -> Space GLfloat Sprite -> IO Bool
draw gs = Triangles.draw gs . map (uncurry $ flip Triangles.applyTransform) . runSpace

clear :: IO ()
clear = glClear gl_COLOR_BUFFER_BIT

modulatedSprite :: Real a => AlphaColour a -> V2 Int -> V2 Int -> Texture -> Space Int Sprite
modulatedSprite color pos dim tex = return $! Triangles.sprite tex color pos dim

sprite :: V2 Int -> V2 Int -> Texture -> Space Int Sprite
sprite = modulatedSprite (opaque (white :: Colour GLfloat))

mapTransform :: (Num t, Num u) => (t -> u) -> Space t a -> Space u a
mapTransform f = space . (map.second.fmap) f . runSpace
