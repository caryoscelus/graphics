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
import Linear.V4

-- Attributes for a single vertex
data Attribs =
  Attribs { attribsPos         :: !(V2 GLfloat)
          , attribsTex         :: !(V2 GLfloat)
          , attribsModulateColor  :: !(V4 GLfloat)
          } deriving Show

-- TODO Put this useful indexed monad somewhere else

evalSmartPtr :: (Ptr i -> IO (a, Ptr j)) -> Ptr i -> IO a
evalSmartPtr m ptr = fst <$> m ptr

infixl 1 !>>=, !>>
(!>>=) :: (Ptr i -> IO (a, Ptr j)) -> (a -> Ptr j -> IO (b, Ptr k)) -> Ptr i -> IO (b, Ptr k)
(m !>>= f) ptr = do
  (x, ptr') <- m ptr
  f x ptr'

(!>>) :: (Ptr i -> IO ((), Ptr j)) -> (Ptr j -> IO (b, Ptr k)) -> Ptr i -> IO (b, Ptr k)
a !>> b = a !>>= \() -> b

ixreturn :: a -> Ptr b -> IO (a, Ptr b)
ixreturn x ptr = return (x, ptr)

smartPeek :: Storable a => Ptr a -> IO (a, Ptr b)
smartPeek ptr = do
  x <- peek ptr
  return (x, ptr `plusPtr` sizeOf x)

smartPoke :: Storable a => a -> Ptr a -> IO ((), Ptr b)
smartPoke x ptr = do
  poke ptr x
  return ((), ptr `plusPtr` sizeOf x)

instance Storable Attribs where
  sizeOf    _ = sizeOf (undefined :: V2 GLfloat) * 2 +
                sizeOf (undefined :: V4 GLfloat)
  alignment _ = alignment (undefined :: V2 GLfloat)
  peek (castPtr -> ptr) =
    (`evalSmartPtr` ptr) $
    smartPeek !>>= \pos    ->
    smartPeek !>>= \tex    ->
    smartPeek !>>= \color  ->
    ixreturn $! Attribs pos tex color
  poke (castPtr -> ptr) Attribs{..} =
    (`evalSmartPtr` ptr) $
    smartPoke attribsPos !>>
    smartPoke attribsTex !>>
    smartPoke attribsModulateColor

-- Attributes for a triangle (the corners must be in counterclockwise
-- order)
data TriangleAttribs =
  TriangleAttribs { corner1 :: !Attribs
                  , corner2 :: !Attribs
                  , corner3 :: !Attribs
                  } deriving Show

instance Storable TriangleAttribs where
  sizeOf    _ = sizeOf    (undefined :: Attribs) * 3
  alignment _ = alignment (undefined :: Attribs)
  peek (castPtr -> ptr) =
    liftA3 TriangleAttribs (peek ptr) (peekElemOff ptr 1) (peekElemOff ptr 2)
  poke (castPtr -> ptr) TriangleAttribs{..} = do
    poke ptr corner1
    pokeElemOff ptr 1 corner2
    pokeElemOff ptr 2 corner3

-- Attributes for a quad. Implemented as two triangles with some
-- redundancy.
data QuadAttribs =
  QuadAttribs { upperLeft  :: !TriangleAttribs
              , lowerRight :: !TriangleAttribs
              } deriving Show

instance Storable QuadAttribs where
  sizeOf    _ = sizeOf    (undefined :: TriangleAttribs) * 2
  alignment _ = alignment (undefined :: TriangleAttribs)
  peek (castPtr -> ptr) = liftA2 QuadAttribs (peek ptr) (peekElemOff ptr 1)
  poke (castPtr -> ptr) QuadAttribs{..} =
    poke ptr upperLeft >> pokeElemOff ptr 1 lowerRight

spriteAttribs :: Sprite -> AffineTransform GLfloat -> QuadAttribs
spriteAttribs Sprite{..} t =
  QuadAttribs
  (TriangleAttribs 
   (Attribs ll (V2 spriteLeft  spriteBottom) spriteModulateColor)
   (Attribs ur (V2 spriteRight spriteTop)    spriteModulateColor)
   (Attribs ul (V2 spriteLeft  spriteTop)    spriteModulateColor))
  (TriangleAttribs
   (Attribs ll (V2 spriteLeft  spriteBottom) spriteModulateColor)
   (Attribs lr (V2 spriteRight spriteBottom) spriteModulateColor)
   (Attribs ur (V2 spriteRight spriteTop   ) spriteModulateColor))
  where (ll, ul, lr, ur) = applyFourCorners01 t

bufferBytes :: Num a => a
bufferBytes = 4*1024*1024

bufferLen :: Int
bufferLen = bufferBytes `div` sizeOf (undefined :: QuadAttribs)

data Chunk =
  Chunk { chunkOffset :: !Int
        , chunkTexId  :: !GLuint
        , chunkQuads  :: ![QuadAttribs]
        } deriving Show

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

-- TODO restore the blend function
drawChunks :: VAO -> Program -> VBO -> [Chunk] -> IO Bool
drawChunks vao program vbo chunks =
  saveAndRestore gl_VERTEX_ARRAY_BINDING glBindVertexArray .
  saveAndRestore gl_CURRENT_PROGRAM glUseProgram .
  saveAndRestore gl_ACTIVE_TEXTURE glActiveTexture .
  saveAndRestore gl_TEXTURE_2D (glBindTexture gl_TEXTURE_2D) .
  saveAndRestoreUnless (==0) gl_ARRAY_BUFFER_BINDING (glBindBuffer gl_ARRAY_BUFFER) $ do
    glBindVertexArray vao
    glUseProgram program -- assume that the uniform for the sampler is already set
    glActiveTexture gl_TEXTURE0
    glBindBuffer gl_ARRAY_BUFFER vbo
    srgbWasEnabled <- (== fromIntegral gl_TRUE) <$>
                      glIsEnabled gl_FRAMEBUFFER_SRGB
    unless srgbWasEnabled $ glEnable gl_FRAMEBUFFER_SRGB

    glBlendFunc gl_ONE gl_ONE_MINUS_SRC_ALPHA
    glEnable gl_BLEND
    
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
      rangeBytes = (bufferLen - chunkOffset)   * sizeOf (undefined :: QuadAttribs)
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
  flushCount <- foldM (\off quad -> off + 1 <$ pokeElemOff ptr off quad) 0 chunkQuads
  glFlushMappedBufferRange gl_ARRAY_BUFFER 0 . fromIntegral $
    flushCount * sizeOf (undefined :: QuadAttribs)
  unmapSuccess <- glUnmapBuffer gl_ARRAY_BUFFER
  let shouldDraw = unmapSuccess == fromIntegral gl_TRUE
  when shouldDraw . glDrawArrays gl_TRIANGLES (fromIntegral chunkOffset) $
    fromIntegral flushCount * 6 -- each sprite has 6 verts
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
  \attribute float blendFactor;\
  \attribute vec4 blendColor;\

  \varying vec2 texcoord;\
  \varying vec4 color;\

  \void main()\
  \{\
  \  gl_Position = vec4(position, 0.0, 1.0);\
  \  texcoord = texPosition;\
  \  color = blendColor;\
  \}"

fShader :: ByteString
fShader =
  "#version 110\n\

  \uniform sampler2D tex;\

  \varying vec2 texcoord;\
  \varying vec4 color;\

  \void main()\
  \{\
  \  gl_FragColor = texture2D(tex, texcoord) * color;\
  \}"

initializeGraphics :: IO GraphicsState
initializeGraphics =
  saveAndRestore gl_VERTEX_ARRAY_BINDING glBindVertexArray .
  saveAndRestoreUnless (==0) gl_ARRAY_BUFFER_BINDING (glBindBuffer gl_ARRAY_BUFFER_BINDING) .
  saveAndRestore gl_CURRENT_PROGRAM glUseProgram $ do
    vao <- glGen glGenVertexArrays
    vbo <- glGen glGenBuffers

    glBindVertexArray vao  
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER bufferBytes nullPtr gl_STREAM_DRAW

    vs <- compileShader vShader gl_VERTEX_SHADER
    fs <- compileShader fShader gl_FRAGMENT_SHADER
    prog <- linkProgram vs fs
    glUseProgram prog

    texUID <- useAsCString "tex" $ glGetUniformLocation prog . castPtr
    glUniform1i texUID 0

    vPosition <- fmap fromIntegral . useAsCString "position" $
                 glGetAttribLocation prog . castPtr
    vTexPosition <- fmap fromIntegral . useAsCString "texPosition" $
                    glGetAttribLocation prog . castPtr
    vModulateColor <- fmap fromIntegral . useAsCString "blendColor" $
                      glGetAttribLocation prog . castPtr

    glVertexAttribPointer vPosition 2 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attribs)) $ intPtrToPtr 0
    glVertexAttribPointer vTexPosition 2 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attribs)) . intPtrToPtr .
      fromIntegral $ sizeOf (undefined :: V2 GLfloat)
    glVertexAttribPointer vModulateColor 4 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attribs)) . intPtrToPtr .
      fromIntegral $ sizeOf (undefined :: V2 GLfloat) * 2

    glEnableVertexAttribArray vPosition
    glEnableVertexAttribArray vTexPosition
    glEnableVertexAttribArray vModulateColor

    return $! GraphicsState vbo vao prog

draw :: GraphicsState -> [(Sprite, AffineTransform GLfloat)] -> IO Bool
draw GraphicsState{..} = drawChunks vao program vbo . chunksToDraw
