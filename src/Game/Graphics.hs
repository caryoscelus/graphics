{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Graphics
       ( Space ()
       , mapTransform
       , transform, translate, rotate, scale, shear, reflect
       , Sampling (..), Texture ()
       , texture, texturePremultiplied, loadTexture, loadTexturePremultiplied
       , Sprite (), sprite, modulatedSprite
       , GraphicsState (), initializeGraphics, draw, clear
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer
import Data.Colour
import Data.Colour.Names (white)
import Data.Foldable
import Data.Traversable
import Data.Word
import Game.Graphics.AffineTransform (AffineTransform)
import Game.Graphics.Sprite hiding (modulatedSprite)
import Game.Graphics.Stream (GraphicsState, initializeGraphics)
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

import qualified Game.Graphics.AffineTransform as Transform
import qualified Game.Graphics.Sprite          as Sprite
import qualified Game.Graphics.Stream          as Stream

-- TODO (<|>) stacks the right argument on top of the left argument. I
-- think I want it to be the other way around.

newtype Space c a = Space { unSpace :: WriterT (AffineTransform c) [] a }
                  deriving ( Functor, Foldable, Traversable, Applicative
                           , Alternative, Monad, MonadPlus, MonadFix
                           )

runSpace :: Space c a -> [(a, AffineTransform c)]
runSpace = toList . runWriterT . unSpace

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

draw :: GraphicsState -> Space GLfloat Sprite -> IO Bool
draw !gs = Stream.draw gs . runSpace

clear :: IO ()
clear = glClear gl_COLOR_BUFFER_BIT

modulatedSprite :: Real a => AlphaColour a -> V2 Word -> V2 Word -> Texture -> Space Int Sprite
modulatedSprite color pos dim tex = do
  translate (V2 (-w `div` 2) (-h `div` 2))
  scale (V2 w h)
  return $! Sprite.modulatedSprite color pos dim tex
  where V2 w h = fmap fromIntegral dim

sprite :: V2 Word -> V2 Word -> Texture -> Space Int Sprite
sprite = modulatedSprite (opaque (white :: Colour GLfloat))

mapTransform :: (t -> u) -> Space t a -> Space u a
mapTransform f = Space . WriterT . (fmap.second.fmap) f . runWriterT . unSpace
