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
       , GraphicsState (), initializeGraphics, draw, clear
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.State.Strict
import Data.Colour
import Data.Colour.Names (white)
import Data.Foldable
import Data.Monoid
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

-- TODO Foldable and Traversable (yes, it can be done, since this is
-- actually a WriterT in disguise!)

newtype Space c a = Space { unSpace :: StateT (AffineTransform c) [] a }
                  deriving ( Functor, {- Foldable, Traversable, -} Applicative
                           , Alternative, Monad, MonadPlus, MonadFix
                           )

runSpace :: Num c => Space c a -> [(a, AffineTransform c)]
runSpace = toList . (`runStateT` mempty) . unSpace

-- TODO put these into a MonadSpace class?

transform :: Num c => AffineTransform c -> Space c ()
transform = Space . modify . flip mappend

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
draw gs = Stream.draw gs . runSpace

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

mapTransform :: (Num t, Num u) => (t -> u) -> Space t a -> Space u a
mapTransform f =
  Space . StateT . (\xs a -> map (\(x, a') -> (x, a <> fmap f a')) xs) .
  (`runStateT` mempty) . unSpace
