{-# LANGUAGE RankNTypes #-}
module Game.Graphics where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Linear.V2
import Text.Show

import Game.AffineTransform as Transform

-- CR jmcarthur: It is not immediately clear whether this representation is
-- actually a win or a loss in terms of efficiency, especially in terms of
-- duplication of work.
newtype Space c a =
  Space { runSpace :: forall r. Monoid r => (AffineTransform c -> a -> r) -> r }

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

instance Functor (Space c) where
  {-# INLINE fmap #-}
  fmap f (Space s) = Space $ \k -> s $ \t -> k t . f

instance Num c => Applicative (Space c) where
  {-# INLINE pure #-}
  pure x = Space $ \k -> k mempty x
  {-# INLINE (<*>) #-}
  Space sf <*> Space sx =
    Space $ \k -> sf $ \t f -> sx $ \t' -> k (t <> t') . f

instance Num c => Alternative (Space c) where
  {-# INLINE empty #-}
  empty = Space mempty
  {-# INLINE (<|>) #-}
  Space f <|> Space g = Space $ f <> g

instance Num c => Monad (Space c) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  Space s >>= f = Space $ \k -> s $ \t x -> case f x of
    Space s' -> s' $ \t' -> k $ t <> t'

instance Num c => MonadPlus (Space c) where
  {-# INLINE mzero #-}
  mzero = empty
  {-# INLINE mplus #-}
  mplus = (<|>)

transform :: AffineTransform c -> Space c ()
{-# INLINE transform #-}
transform t = Space $ \k -> k t ()

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
