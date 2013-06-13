{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Game.Graphics.AffineTransform
       ( AffineTransform ()
       , translate, rotate, scale, shear, reflect, invert
       , apply, applyFourCorners01
       ) where


import Data.Monoid
import Linear.V2

-- TODO applyFourCorners01 may be fairly implementation-revealing and
-- should probably be hidden somehow (an Internal module?)

-- TODO It's not really right for this to be a Functor. There should
-- just be some primitives for converting.

-- TODO Consider making these fields monomorphic or at least
-- specializing for just a few types. They account for a lot of heap
-- usage, right now.

data AffineTransform a = A2D !a !a !a
                             !a !a !a
                           -- 0  0  1
              deriving (Eq, Ord, Read, Show, Functor)

instance Num a => Monoid (AffineTransform a) where
  {-# INLINE mempty #-}
  mempty = A2D 1 0 0
               0 1 0
  {-# INLINE mappend #-}
  A2D a11 a12 a13
      a21 a22 a23 `mappend` A2D b11 b12 b13
                                b21 b22 b23 = A2D (a11*b11 + a12*b21) (a11*b12 + a12*b22) (a11*b13 + a12*b23 + a13)
                                                  (a21*b11 + a22*b21) (a21*b12 + a22*b22) (a21*b13 + a22*b23 + a23)

translate :: Num a => V2 a -> AffineTransform a
{-# INLINE translate #-}
translate (V2 x y) = A2D 1 0 x
                         0 1 y

rotate :: Floating a => a -> AffineTransform a
{-# INLINE rotate #-}
rotate !r = A2D cosr (-sinr) 0
               sinr   cosr  0
  where !cosr = cos r
        !sinr = sin r

scale :: Num a => V2 a -> AffineTransform a
{-# INLINE scale #-}
scale (V2 x y) = A2D x 0 0
                     0 y 0

shear :: Num a => V2 a -> AffineTransform a
{-# INLINE shear #-}
shear (V2 x y) = A2D 1 x 0
                     y 1 0

-- | Reflect about a unit vector
reflect :: Num a => V2 a -> AffineTransform a
{-# INLINE reflect #-}
reflect (V2 x y) = A2D (x2 - y2) twoXY     0
                       twoXY     (y2 - x2) 0
  where !twoXY = 2 * x * y
        !x2    = x * x
        !y2    = y * y

invert :: Fractional a => AffineTransform a -> AffineTransform a
{-# INLINE invert #-}
invert (A2D a11 a12 a13
            a21 a22 a23) = A2D (a22/det) (-a12/det) ((a12*a23 - a22*a13) / det)
                               (-a21/det) (a11/det) ((a21*a13 - a11*a23) / det)
  where !det = a11*a22 - a12 * a21

apply :: Num a => AffineTransform a -> V2 a -> V2 a
{-# INLINE apply #-}
apply (A2D a11 a12 a13
           a21 a22 a23) (V2 b1 b2) = V2 (a11*b1 + a12*b2 + a13) (a21*b1 + a22*b2 + a23)

-- | [[applyFourCorners01 a]] = (apply a (0,0), apply a (0,1), apply a (1,0), apply a (1,1))
applyFourCorners01 :: Num a => AffineTransform a -> (V2 a, V2 a, V2 a, V2 a)
{-# INLINE applyFourCorners01 #-}
applyFourCorners01 (A2D a11 a12 a13
                        a21 a22 a23) =
  (zz, zzzo, zz + oz, zzzo + oz)
  where !zz = V2 a13 a23
        !zo = V2 a12 a22
        !oz = V2 a11 a21
        !zzzo = zz + zo
