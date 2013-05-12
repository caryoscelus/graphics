{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
module Game.Graphics where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import Linear.V2
import Text.Show

import Game.AffineTransform as Transform

-- TODO Reorganize modules. They don't really make any sense right now.

-- CR jmcarthur: It is not immediately clear whether this representation is
-- actually a win or a loss in terms of efficiency, especially in terms of
-- duplication of work.
newtype SpaceT c m a =
  SpaceT { runSpaceT :: forall r. Monoid r => (AffineTransform c -> a -> m r) -> m r }

type Space c a = SpaceT c Identity a

runSpace :: Monoid r => Space c a -> (AffineTransform c -> a -> r) -> r
{-# INLINE runSpace #-}
runSpace (SpaceT s) f = runIdentity . s $ (fmap.fmap) Identity f

-- TODO support Show (m a) instead of just Show a
instance (Show a, Show c) => Show (Space c a) where
  showsPrec p s =
    showParen (p > 10) $
    showString "asum " .
    showListWith id (runSpace s showOne)
    where showOne t x =
            pure $
            showsPrec 4 x .
            showString " <$ " .
            showString "transform " .
            showsPrec 11 t

instance Functor (SpaceT c m) where
  {-# INLINE fmap #-}
  fmap f (SpaceT s) = SpaceT $ \k -> s $ \t -> k t . f

instance Num c => Applicative (SpaceT c m) where
  {-# INLINE pure #-}
  pure x = SpaceT $ \k -> k mempty x
  {-# INLINE (<*>) #-}
  SpaceT sf <*> SpaceT sx =
    SpaceT $ \k -> sf $ \t f -> sx $ \t' -> k (t <> t') . f

instance (Applicative m, Num c) => Alternative (SpaceT c m) where
  {-# INLINE empty #-}
  empty = SpaceT $ (pure.pure) mempty
  {-# INLINE (<|>) #-}
  SpaceT f <|> SpaceT g = SpaceT $ (liftA2.liftA2) (<>) f g

instance Num c => Monad (SpaceT c m) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  SpaceT s >>= f = SpaceT $ \k -> s $ \t x -> case f x of
    SpaceT s' -> s' $ \t' -> k $ t <> t'

instance (Monad m, Num c) => MonadPlus (SpaceT c m) where
  {-# INLINE mzero #-}
  mzero = SpaceT $ (return.return) mempty
  {-# INLINE mplus #-}
  SpaceT f `mplus` SpaceT g = SpaceT $ (liftM2.liftM2) (<>) f g

transform :: AffineTransform c -> SpaceT c m ()
{-# INLINE transform #-}
transform t = SpaceT $ \k -> k t ()

translate :: Num c => V2 c -> SpaceT c m ()
{-# INLINE translate #-}
translate = transform . Transform.translate

rotate :: Floating c => c -> SpaceT c m ()
{-# INLINE rotate #-}
rotate = transform . Transform.rotate

scale :: Num c => V2 c -> SpaceT c m ()
{-# INLINE scale #-}
scale = transform . Transform.scale

shear :: Num c => V2 c -> SpaceT c m ()
{-# INLINE shear #-}
shear = transform . Transform.shear

reflect :: Num c => V2 c -> SpaceT c m ()
{-# INLINE reflect #-}
reflect = transform . Transform.reflect
