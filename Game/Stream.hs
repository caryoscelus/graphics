{-# OPTIONS -fexpose-all-unfoldings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Stream where

import Control.Applicative
import Data.List
import Foreign.Ptr
import Foreign.Storable
import Game.AffineTransform
import Game.Sprite
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

data Attribs =
  Attribs { xPos :: {-# UNPACK #-} !GLfloat
          , yPos :: {-# UNPACK #-} !GLfloat
          , xTex :: {-# UNPACK #-} !GLfloat
          , yTex :: {-# UNPACK #-} !GLfloat
          }

instance Storable Attribs where
  sizeOf    _ = sizeOf    (undefined :: GLfloat) * 4
  alignment _ = alignment (undefined :: GLfloat)
  peekElemOff (castPtr -> ptr) off =
    Attribs                 <$>
    peekElemOff ptr  off    <*>
    peekElemOff ptr (off+1) <*>
    peekElemOff ptr (off+2) <*>
    peekElemOff ptr (off+3)
  pokeElemOff (castPtr -> ptr) off Attribs{..} = do
    pokeElemOff ptr  off    xPos
    pokeElemOff ptr (off+1) yPos
    pokeElemOff ptr (off+2) xTex
    pokeElemOff ptr (off+3) yTex

data TriangleAttribs =
  TriangleAttribs { corner1 :: {-# UNPACK #-} !Attribs
                  , corner2 :: {-# UNPACK #-} !Attribs
                  , corner3 :: {-# UNPACK #-} !Attribs
                  }

instance Storable TriangleAttribs where
  sizeOf    _ = sizeOf    (undefined :: Attribs) * 3
  alignment _ = alignment (undefined :: Attribs)
  peekElemOff (castPtr -> ptr) off =
    TriangleAttribs         <$>
    peekElemOff ptr  off    <*>
    peekElemOff ptr (off+1) <*>
    peekElemOff ptr (off+2)
  pokeElemOff (castPtr -> ptr) off TriangleAttribs{..} = do
    pokeElemOff ptr  off    corner1
    pokeElemOff ptr (off+1) corner2
    pokeElemOff ptr (off+2) corner3 

data QuadAttribs =
  QuadAttribs { upperLeft  :: {-# UNPACK #-} !TriangleAttribs
              , lowerRight :: {-# UNPACK #-} !TriangleAttribs
              }

instance Storable QuadAttribs where
  sizeOf    _ = sizeOf    (undefined :: TriangleAttribs) * 3
  alignment _ = alignment (undefined :: TriangleAttribs)
  peekElemOff (castPtr -> ptr) off =
    QuadAttribs             <$>
    peekElemOff ptr  off    <*>
    peekElemOff ptr (off+1)
  pokeElemOff (castPtr -> ptr) off QuadAttribs{..} = do
    pokeElemOff ptr  off    upperLeft
    pokeElemOff ptr (off+1) lowerRight

pokeSpriteOff :: Ptr QuadAttribs -> Int -> Sprite -> AffineTransform GLfloat -> IO ()
pokeSpriteOff ptr off Sprite{..} t =
  pokeElemOff ptr off $
  QuadAttribs
  (TriangleAttribs
   (Attribs ulx uly spriteLeft  spriteTop)
   (Attribs urx ury spriteRight spriteTop)
   (Attribs llx lly spriteLeft  spriteBottom))
  (TriangleAttribs
   (Attribs urx ury spriteRight spriteTop)
   (Attribs lrx lry spriteRight spriteBottom)
   (Attribs llx lly spriteLeft  spriteBottom))
  where (V2 llx lly, V2 ulx uly, V2 lrx lry, V2 urx ury) = applyFourCorners01 t

bufferBytes :: Num a => a
bufferBytes = 8*1024*1024

-- TODO this is a pretty ugly function
chunksToDraw :: [(Sprite, AffineTransform GLfloat)] -> [[(Int, Sprite, AffineTransform GLfloat)]]
chunksToDraw =
  groupBy (\(_,x,_) (j,y,_) -> spriteTexId x == spriteTexId y && j /= 0) .
  zipWith (\i (s, t) -> (i, s, t))
  (cycle [0..(bufferBytes `div` sizeOf (undefined :: QuadAttribs))])

