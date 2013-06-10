{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields   #-}
{-# LANGUAGE DeriveFunctor #-}
module Game.Graphics.AffineTransform
       ( AffineTransform (..)
       , translate, rotate, scale, shear, reflect, invert
       , apply, applyFourCorners01
       ) where


import Data.Monoid
import Linear.V2

-- TODO applyFourCorners01 may be fairly implementation-revealing and
-- should probably be hidden somehow (an Internal module?)

-- TODO It's not really right for this to be a Functor. There should
-- just be some primitives for converting.

-- TODO Consider making these fields monomorphic.

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
rotate r = A2D cosr (-sinr) 0
               sinr   cosr  0
  where cosr = cos r
        sinr = sin r

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
  where twoXY = 2 * x * y
        x2    = x * x
        y2    = y * y

invert :: Fractional a => AffineTransform a -> AffineTransform a
{-# INLINE invert #-}
invert (A2D a11 a12 a13
            a21 a22 a23) = A2D (a22/det) (-a12/det) ((a12*a23 - a22*a13) / det)
                               (-a21/det) (a11/det) ((a21*a13 - a11*a23) / det)
  where det = a11*a22 - a12 * a21

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
  where zz = V2 a13 a23
        zo = V2 a12 a22
        oz = V2 a11 a21
        zzzo = zz + zo

-- {-# OPTIONS -fexpose-all-unfoldings #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- module Game.Graphics.AffineTransform
--        ( AffineTransform ()
--        , translate , rotate , scale , shear , reflect , invert, apply
--        , applyFourCorners01
--        ) where

-- -- TODO applyFourCorners01 may be fairly implementation-revealing and
-- -- should probably be hidden somehow (an Internal module?)

-- import Control.Applicative
-- import Data.Monoid
-- import Linear.Epsilon
-- import Linear.Matrix
-- import Linear.V2
-- import Linear.V3

-- -- TODO We could use a more efficient representation. The bottom row
-- -- can probably be left off. It would just require some more manual
-- -- work to implement all the operations. Eventually, I want to
-- -- actually do this.

-- -- TODO Moar instances.

-- newtype AffineTransform a = AffineTransform (V3 (V3 a))
--                           deriving (Functor, Show)

-- instance Num a => Monoid (AffineTransform a) where
--   mempty = AffineTransform eye3
--   AffineTransform a `mappend` AffineTransform b = AffineTransform $ a !*! b

-- bottomRow :: Num a => V3 a
-- bottomRow = V3 0 0 1

-- translate :: Num a => V2 a -> AffineTransform a
-- translate (V2 x y) =
--   AffineTransform (V3 (V3 1 0 x)
--                       (V3 0 1 y)
--                       bottomRow)

-- rotate :: Floating a => a -> AffineTransform a
-- rotate r =
--   AffineTransform (V3 (V3 cosr (-sinr) 0)
--                       (V3 sinr   cosr  0)
--                       bottomRow)
--   where cosr = cos r
--         sinr = sin r

-- scale :: Num a => V2 a -> AffineTransform a
-- scale (V2 x y) =
--   AffineTransform (V3 (V3 x 0 0)
--                       (V3 0 y 0)
--                       bottomRow)

-- shear :: Num a => V2 a -> AffineTransform a
-- shear (V2 x y) =
--   AffineTransform (V3 (V3 1 x 0)
--                       (V3 y 1 0)
--                       bottomRow)

-- reflect :: Num a => V2 a -> AffineTransform a
-- reflect (V2 x y) =
--   AffineTransform (V3 (V3 (x2-y2) twoXY   0)
--                       (V3 twoXY   (y2-x2) 0)
--                       bottomRow)
--   where twoXY = 2 * x * y
--         x2    = x * x
--         y2    = y * y

-- invert :: (Floating a, Epsilon a) => AffineTransform a -> Maybe (AffineTransform a)
-- invert (AffineTransform a) = AffineTransform <$> inv33 a

-- apply :: Num a => AffineTransform a -> V2 a -> V2 a
-- apply (AffineTransform a) (V2 x y) =
--   let V3 x' y' _ = a !* V3 x y 1
--   in V2 x' y'

-- -- | @[[applyFourCorners01 a]] = (apply a (0,0), apply a (0,1), apply a (1,0), apply a (1,1))@
-- applyFourCorners01 :: (Num a, Eq a) => AffineTransform a -> (V2 a, V2 a, V2 a, V2 a)
-- applyFourCorners01 (AffineTransform (V3 (V3 a11 a12 a13)
--                                         (V3 a21 a22 a23)
--                                         (V3 0   0   1))) =
--   (zz, zzzo, zz + oz, zzzo + oz)
--   where zz = V2 a13 a23
--         zo = V2 a12 a22
--         oz = V2 a11 a21
--         zzzo = zz + zo
-- applyFourCorners01 _ = error "bottom row is not 0 0 1"
