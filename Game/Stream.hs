{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Stream where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import Foreign.Ptr
import Foreign.Storable
import Game.AffineTransform
import Game.Sprite
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

-- Attributes for a single vertex
data Attribs =
  Attribs { xPos :: !GLfloat
          , yPos :: !GLfloat
          , xTex :: !GLfloat
          , yTex :: !GLfloat
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

-- Attributes for a triangle (the corners must be in clockwise order)
data TriangleAttribs =
  TriangleAttribs { corner1 :: !Attribs
                  , corner2 :: !Attribs
                  , corner3 :: !Attribs
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

-- Attributes for a quad. Implemented as two triangles with some
-- redundancy.
data QuadAttribs =
  QuadAttribs { upperLeft  :: !TriangleAttribs
              , lowerRight :: !TriangleAttribs
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

spriteAttribs :: Sprite -> AffineTransform GLfloat -> QuadAttribs
spriteAttribs Sprite{..} t =
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
bufferBytes = 4*1024*1024

bufferLen :: Int
bufferLen = bufferBytes `div` sizeOf (undefined :: QuadAttribs)

data Chunk =
  Chunk { chunkOffset :: !Int
        , chunkTexId  :: !GLuint
        , chunkQuads  :: ![QuadAttribs]
        }

-- Given a list of sprites to draw, divide it into chunks, each of
-- which corresponds to a draw call. Every time we switch textures,
-- that's a draw call. Every time we reach the end of the buffer,
-- that's a draw call and a buffer orphaning.
chunksToDraw :: [(Sprite, AffineTransform GLfloat)] -> [Chunk]
chunksToDraw =
  map (\xs@((i, (s, _)):_) -> Chunk i (spriteTexId s) $
                              map (uncurry spriteAttribs . snd) xs) .
  groupBy (\(_,(x,_)) (j,(y,_)) -> spriteTexId x == spriteTexId y && j /= 0) .
  zip (cycle [0..bufferLen])

type VBO = GLuint
type VAO = GLuint
type Program = GLuint
type Uniform = GLint

drawChunks :: VAO -> Program -> VBO -> Uniform -> [Chunk] -> IO ()
drawChunks vao program vbo texUniform chunks = do
  glBindVertexArray vao
  glUseProgram program
  glBindBuffer gl_ARRAY_BUFFER vbo
  foldM (\ !success chunk -> (&&success) <$> drawChunk texUniform chunk) True chunks
  -- TODO restore state
  undefined

-- TODO indexed draws?

-- | Draw the given chunk. Returns false if the draw call was skipped
-- due to a buffer mapping error. This should be rare and temporary,
-- so instead of trying to handle it we just report it.
drawChunk :: Uniform -> Chunk -> IO Bool
drawChunk texUniform Chunk{..} = do
  let offsetBytes = fromIntegral $ chunkOffset * sizeOf (undefined :: QuadAttribs)
      rangeBytes = (bufferLen - chunkOffset) * sizeOf (undefined :: QuadAttribs)
      invalidateBufferBit
        | chunkOffset == 0 = gl_MAP_INVALIDATE_BUFFER_BIT
        | otherwise   = 0
  glBindTexture gl_TEXTURE_2D chunkTexId
  glUniform1i texUniform 0
  glActiveTexture gl_TEXTURE0
  ptr <- glMapBufferRange gl_ARRAY_BUFFER offsetBytes (fromIntegral rangeBytes) $
         gl_MAP_WRITE_BIT            .|.
         gl_MAP_INVALIDATE_RANGE_BIT .|.
         gl_MAP_FLUSH_EXPLICIT_BIT   .|.
         gl_MAP_UNSYNCHRONIZED_BIT   .|.
         invalidateBufferBit
  flushCount <- foldM (\off a -> off + 1 <$ pokeElemOff ptr off a) 0 chunkQuads
  glFlushMappedBufferRange gl_ARRAY_BUFFER 0 . fromIntegral $
    flushCount * sizeOf (undefined :: Attribs)
  unmapSuccess <- glUnmapBuffer gl_ARRAY_BUFFER
  let shouldDraw = unmapSuccess == fromIntegral gl_TRUE
  when shouldDraw . glDrawArrays gl_TRIANGLES (fromIntegral chunkOffset) $
    fromIntegral flushCount
  return shouldDraw
