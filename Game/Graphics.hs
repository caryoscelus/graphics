{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Graphics
       ( Space ()
       , transform, translate, rotate, scale, shear, reflect
       , Texture (), Sprite (), texture, sprite
       , GraphicsState (), draw
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Traversable
import Graphics.Rendering.OpenGL.Raw.Core31 (GLfloat, GLuint)
import Linear.V2

import Game.Graphics.AffineTransform (AffineTransform)
import qualified Game.Graphics.AffineTransform as Transform
import Game.Graphics.Sprite
import Game.Graphics.Stream

-- TODO Some sort of DList-like monad instead of []
-- TODO A more specialized WriterT (maybe the whole thing should just be specialized)
newtype Space c a = Space { unSpace :: WriterT (AffineTransform c) [] a }
                  deriving ( Functor, Applicative, Monad, Alternative
                           , MonadPlus, MonadFix, Foldable, Traversable
                           )

runSpace :: Space c a -> [(a, AffineTransform c)]
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

data GraphicsState =
  GraphicsState { vbo     :: !GLuint
                , vao     :: !GLuint
                , program :: !GLuint
                }

draw :: GraphicsState -> Space GLfloat Sprite -> IO Bool
draw GraphicsState{..} = drawChunks vao program vbo . chunksToDraw . runSpace
