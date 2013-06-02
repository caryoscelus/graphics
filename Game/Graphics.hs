{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
module Game.Graphics where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Traversable
import Linear.V2

import Game.AffineTransform as Transform

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
