{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Graphics.Stream (GraphicsState (), initializeGraphics, GLfloat, draw) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString, useAsCString)
import Data.List
import Foreign.Ptr
import Foreign.Storable
import Game.Graphics.AffineTransform
import Game.Graphics.Shader
import Game.Graphics.Sprite
import Game.Graphics.Utils
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear.V2

-- Attributes for a single vertex
data Attribs =
  Attribs { attribsPos :: !(V2 GLfloat)
          , attribsTex :: !(V2 GLfloat)
          }

instance Storable Attribs where
  sizeOf    _ = sizeOf    (undefined :: V2 GLfloat) * 2
  alignment _ = alignment (undefined :: V2 GLfloat)
  peekElemOff (castPtr -> ptr) off =
    Attribs                 <$>
    peekElemOff ptr  off    <*>
    peekElemOff ptr (off+1)
  pokeElemOff (castPtr -> ptr) off Attribs{..} = do
    pokeElemOff ptr  off    attribsPos
    pokeElemOff ptr (off+1) attribsTex

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
   (Attribs ul $ V2 spriteLeft  spriteTop)
   (Attribs ur $ V2 spriteRight spriteTop)
   (Attribs ll $ V2 spriteLeft  spriteBottom))
  (TriangleAttribs
   (Attribs ur $ V2 spriteRight spriteTop)
   (Attribs lr $ V2 spriteRight spriteBottom)
   (Attribs ll $ V2 spriteLeft  spriteBottom))
  where (ll, ul, lr, ur) = applyFourCorners01 t

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

drawChunks :: VAO -> Program -> VBO -> [Chunk] -> IO Bool
drawChunks vao program vbo chunks =
  saveAndRestore gl_VERTEX_ARRAY_BINDING glBindVertexArray .
  saveAndRestore gl_CURRENT_PROGRAM glUseProgram .
  saveAndRestore gl_ACTIVE_TEXTURE glActiveTexture .
  saveAndRestore gl_TEXTURE_2D (glBindTexture gl_TEXTURE_2D) .
  saveAndRestore gl_ARRAY_BUFFER_BINDING (glBindBuffer gl_ARRAY_BUFFER) $ do
    glBindVertexArray vao
    glUseProgram program -- assume that the uniform for the sampler is already set
    glActiveTexture gl_TEXTURE0
    glBindBuffer gl_ARRAY_BUFFER vbo
    srgbWasEnabled <- (== fromIntegral gl_TRUE) <$>
                      glIsEnabled gl_FRAMEBUFFER_SRGB
    unless srgbWasEnabled $ glEnable gl_FRAMEBUFFER_SRGB
    drewCleanly <- foldM (\ !success chunk -> (&&success) <$> drawChunk chunk)
                   True chunks
    unless srgbWasEnabled $ glDisable gl_FRAMEBUFFER_SRGB  
    return drewCleanly

-- TODO indexed draws?

-- | Draw the given chunk. Returns false if the draw call was skipped
-- due to a buffer mapping error. This should be rare and temporary,
-- so instead of trying to handle it we just report it.
drawChunk :: Chunk -> IO Bool
drawChunk Chunk{..} = do
  let offsetBytes = fromIntegral $ chunkOffset * sizeOf (undefined :: QuadAttribs)
      rangeBytes = (bufferLen - chunkOffset) * sizeOf (undefined :: QuadAttribs)
      invalidateBufferBit
        | chunkOffset == 0 = gl_MAP_INVALIDATE_BUFFER_BIT
        | otherwise        = 0
  glBindTexture gl_TEXTURE_2D chunkTexId
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

data GraphicsState =
  GraphicsState { vbo     :: !GLuint
                , vao     :: !GLuint
                , program :: !GLuint
                }

vShader :: ByteString
vShader =
  "#version 110\n\

  \attribute vec2 position;\
  \attribute vec2 texPosition;\

  \varying vec2 texcoord;\

  \void main()\
  \{\
  \  gl_Position = vec4(position, 0.0, 1.0);\
  \  texcoord = texPosition;\
  \}"

fShader :: ByteString
fShader =
  "#version 110\n\

  \uniform sampler2D tex;\

  \varying vec2 texcoord;\

  \void main()\
  \{\
  \  gl_FragColor = texture2D(texture, texcoord);\
  \}"

initializeGraphics :: IO GraphicsState
initializeGraphics =
  saveAndRestore gl_VERTEX_ARRAY_BINDING glBindVertexArray .
  saveAndRestore gl_ARRAY_BUFFER_BINDING (glBindBuffer gl_ARRAY_BUFFER_BINDING) $ do
    vao <- glGen glGenVertexArrays
    vbo <- glGen glGenBuffers

    glBindVertexArray vao  
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER bufferBytes nullPtr gl_STREAM_DRAW

    vs <- compileShader vShader gl_VERTEX_SHADER
    fs <- compileShader fShader gl_FRAGMENT_SHADER
    prog <- linkProgram vs fs

    texUID <- useAsCString "tex" $ glGetUniformLocation prog . castPtr
    glUniform1i texUID 0

    vPosition <- fmap fromIntegral . useAsCString "position" $
                 glGetAttribLocation prog . castPtr
    vTexPosition <- fmap fromIntegral . useAsCString "texPosition" $
                    glGetAttribLocation prog . castPtr

    glVertexAttribPointer vPosition 2 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attribs)) $ intPtrToPtr 0
    glVertexAttribPointer vTexPosition 2 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attribs)) . intPtrToPtr .
      fromIntegral $ sizeOf (undefined :: V2 GLfloat)
    glEnableVertexAttribArray vPosition
    glEnableVertexAttribArray vTexPosition

    return $! GraphicsState vbo vao prog

draw :: GraphicsState -> [(Sprite, AffineTransform GLfloat)] -> IO Bool
draw GraphicsState{..} = drawChunks vao program vbo . chunksToDraw
