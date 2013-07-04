{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields   #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
module Game.Graphics.AffineTransform
       ( AffineTransform ()
       , translate, rotate, scale, shear, reflect, invert
       , apply, applyFourCorners01
       ) where

import Data.Monoid
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

-- TODO applyFourCorners01 may be fairly implementation-revealing and
-- should probably be hidden somehow (an Internal module?)

data AffineTransform = A2D !GLfloat !GLfloat !GLfloat
                           !GLfloat !GLfloat !GLfloat
                         -- 0  0  1
              deriving (Eq, Ord, Read, Show)

instance Monoid AffineTransform where
  {-# INLINE mempty #-}
  mempty = A2D 1 0 0
               0 1 0

  {-# INLINE mappend #-}
  A2D a11 a12 a13
      a21 a22 a23 `mappend` A2D b11 b12 b13
                                b21 b22 b23 = A2D (a11*b11 + a12*b21) (a11*b12 + a12*b22) (a11*b13 + a12*b23 + a13)
                                                  (a21*b11 + a22*b21) (a21*b12 + a22*b22) (a21*b13 + a22*b23 + a23)

translate :: V2 GLfloat -> AffineTransform
{-# INLINE translate #-}
translate (V2 x y) = A2D 1 0 x
                         0 1 y

rotate :: GLfloat -> AffineTransform
{-# INLINE rotate #-}
rotate !r = A2D cosr (-sinr) 0
                sinr   cosr  0
  where !cosr = cos r
        !sinr = sin r

scale :: V2 GLfloat -> AffineTransform
{-# INLINE scale #-}
scale (V2 !x !y) = A2D x 0 0
                       0 y 0

shear :: V2 GLfloat -> AffineTransform
{-# INLINE shear #-}
shear (V2 !x !y) = A2D 1 x 0
                       y 1 0

-- | Reflect about a unit vector
reflect :: V2 GLfloat -> AffineTransform
{-# INLINE reflect #-}
reflect (V2 !x !y) = A2D (x2 - y2) twoXY     0
                         twoXY     (y2 - x2) 0
  where !twoXY = 2 * x * y
        !x2    = x * x
        !y2    = y * y

invert :: AffineTransform -> AffineTransform
{-# INLINE invert #-}
invert (A2D a11 a12 a13
            a21 a22 a23) = A2D (a22/det) (-a12/det) ((a12*a23 - a22*a13) / det)
                               (-a21/det) (a11/det) ((a21*a13 - a11*a23) / det)
  where !det = a11*a22 - a12 * a21

apply :: AffineTransform -> V2 GLfloat -> V2 GLfloat
{-# INLINE apply #-}
apply (A2D a11 a12 a13
           a21 a22 a23) (V2 b1 b2) = V2 (a11*b1 + a12*b2 + a13) (a21*b1 + a22*b2 + a23)

-- | [[applyFourCorners01 a]] = (apply a (0,0), apply a (0,1), apply a (1,0), apply a (1,1))
applyFourCorners01 :: AffineTransform -> (V2 GLfloat, V2 GLfloat, V2 GLfloat, V2 GLfloat)
{-# INLINE applyFourCorners01 #-}
applyFourCorners01 (A2D a11 a12 a13
                        a21 a22 a23) =
  (zz, zzzo, zz + oz, zzzo + oz)
  where !zz = V2 a13 a23
        !zo = V2 a12 a22
        !oz = V2 a11 a21
        !zzzo = zz + zo
