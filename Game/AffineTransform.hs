{-# OPTIONS -fexpose-all-unfoldings #-}
module Game.AffineTransform where

import Control.Applicative
import Data.Monoid
import Linear.Epsilon
import Linear.Matrix
import Linear.V2
import Linear.V3

-- CR jmcarthur: We could use a more efficient representation. The
-- bottom row can probably be left off. It would just require some
-- more manual work to implement all the operations. Eventually, I
-- want to actually do this.

-- CR jmcarthur: Moar instances.
newtype AffineTransform a = AffineTransform (V3 (V3 a)) deriving Show

instance Num a => Monoid (AffineTransform a) where
  mempty = AffineTransform eye3
  AffineTransform a `mappend` AffineTransform b = AffineTransform $ a !*! b

bottomRow :: Num a => V3 a
bottomRow = V3 0 0 1

translate :: Num a => V2 a -> AffineTransform a
translate (V2 x y) =
  AffineTransform (V3 (V3 1 0 x)
                      (V3 0 1 y)
                      bottomRow)

rotate :: Floating a => a -> AffineTransform a
rotate r =
  AffineTransform (V3 (V3 cosr (-sinr) 0)
                      (V3 sinr   cosr  0)
                      bottomRow)
  where cosr = cos r
        sinr = sin r

scale :: Num a => V2 a -> AffineTransform a
scale (V2 x y) =
  AffineTransform (V3 (V3 x 0 0)
                      (V3 0 y 0)
                      bottomRow)

shear :: Num a => V2 a -> AffineTransform a
shear (V2 x y) =
  AffineTransform (V3 (V3 1 x 0)
                      (V3 y 1 0)
                      bottomRow)

reflect :: Num a => V2 a -> AffineTransform a
reflect (V2 x y) =
  AffineTransform (V3 (V3 (x2-y2) twoXY   0)
                      (V3 twoXY   (y2-x2) 0)
                      bottomRow)
  where twoXY = 2 * x * y
        x2    = x * x
        y2    = y * y

invert :: (Floating a, Epsilon a) => AffineTransform a -> Maybe (AffineTransform a)
invert (AffineTransform a) = AffineTransform <$> inv33 a

apply :: Num a => AffineTransform a -> V2 a -> V2 a
apply (AffineTransform a) (V2 x y) =
  let V3 x' y' _ = a !* V3 x y 1
  in V2 x' y'

-- | [[applyFourCorners01 a]] = (apply a (0,0), apply a (0,1), apply a (1,0), apply a (1,1))
applyFourCorners01 :: (Num a, Eq a) => AffineTransform a -> (V2 a, V2 a, V2 a, V2 a)
applyFourCorners01 (AffineTransform (V3 (V3 a11 a12 a13)
                                        (V3 a21 a22 a23)
                                        (V3 0   0   1))) =
  (zz, zzzo, zz + oz, zzzo + oz)
  where zz = V2 a13 a23
        zo = V2 a12 a22
        oz = V2 a11 a21
        zzzo = zz + zo
applyFourCorners01 _ = error "bottom row is not 0 0 1"
