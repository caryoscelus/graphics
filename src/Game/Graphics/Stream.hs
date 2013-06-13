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
import qualified Data.Vector.Storable as Vector
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
  Attribs { attribsPosX      :: !GLfloat
          , attribsPosY      :: !GLfloat
          , attribsTexX      :: !GLfloat
          , attribsTexY      :: !GLfloat
          , attribsModulateR :: !GLfloat
          , attribsModulateG :: !GLfloat
          , attribsModulateB :: !GLfloat
          , attribsModulateA :: !GLfloat
          } deriving Show

instance Storable Attribs where
  sizeOf    _ = sizeOf (undefined :: V2 GLfloat) * 2 +
                sizeOf (undefined :: V4 GLfloat)
  alignment _ = alignment (undefined :: V2 GLfloat)
  peek (castPtr -> !ptr) =
    Attribs           <$>
    peek        ptr   <*>
    peekElemOff ptr 1 <*>
    peekElemOff ptr 2 <*>
    peekElemOff ptr 3 <*>
    peekElemOff ptr 4 <*>
    peekElemOff ptr 5 <*>
    peekElemOff ptr 6 <*>
    peekElemOff ptr 7
  poke (castPtr -> !ptr) Attribs{..} = do
    poke        ptr   attribsPosX
    pokeElemOff ptr 1 attribsPosY
    pokeElemOff ptr 2 attribsTexX
    pokeElemOff ptr 3 attribsTexY
    pokeElemOff ptr 4 attribsModulateR
    pokeElemOff ptr 5 attribsModulateG
    pokeElemOff ptr 6 attribsModulateB
    pokeElemOff ptr 7 attribsModulateA
    
    -- do
    -- poke (castPtr ptr) attribsPos
    -- pokeByteOff (castPtr ptr) (sizeOf attribsPos) attribsTex
    -- pokeByteOff (castPtr ptr) (sizeOf attribsPos + sizeOf attribsTex) attribsModulateColor

-- Attributes for a quad.
data QuadAttribs =
  QuadAttribs { upperLeft  :: !Attribs
              , upperRight :: !Attribs
              , lowerLeft  :: !Attribs
              , lowerRight :: !Attribs
              } deriving Show

instance Storable QuadAttribs where
  sizeOf    _ = sizeOf    (undefined :: Attribs) * 4
  alignment _ = alignment (undefined :: Attribs)
  peek (castPtr -> !ptr) = QuadAttribs <$> peek ptr <*> peekElemOff ptr 1 <*> peekElemOff ptr 2 <*> peekElemOff ptr 3
  poke (castPtr -> !ptr) QuadAttribs{..} = do
    poke ptr upperLeft
    pokeElemOff ptr 1 upperRight
    pokeElemOff ptr 2 lowerLeft
    pokeElemOff ptr 3 lowerRight

spriteAttribs :: Sprite -> AffineTransform GLfloat -> QuadAttribs
spriteAttribs Sprite{..} !t =
  QuadAttribs
   (Attribs ulx uly spriteLeft  spriteTop    spriteModulateR spriteModulateG spriteModulateB spriteModulateA)
   (Attribs urx ury spriteRight spriteTop    spriteModulateR spriteModulateG spriteModulateB spriteModulateA)
   (Attribs llx lly spriteLeft  spriteBottom spriteModulateR spriteModulateG spriteModulateB spriteModulateA)
   (Attribs lrx lry spriteRight spriteBottom spriteModulateR spriteModulateG spriteModulateB spriteModulateA)
  where (V2 !llx !lly, V2 !ulx !uly, V2 !lrx !lry, V2 !urx !ury) = applyFourCorners01 t

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
  zip (cycle [0..bufferLen-1])

data GraphicsState =
  GraphicsState { vbo     :: !GLuint
                , ibo     :: !GLuint
                , vao     :: !GLuint
                , program :: !GLuint
                }

-- TODO restore the blend function
drawChunks :: GraphicsState -> [Chunk] -> IO Bool
drawChunks GraphicsState{..} chunks =
  saveAndRestore gl_VERTEX_ARRAY_BINDING glBindVertexArray .
  saveAndRestore gl_CURRENT_PROGRAM glUseProgram .
  saveAndRestore gl_ACTIVE_TEXTURE glActiveTexture .
  saveAndRestore gl_TEXTURE_2D (glBindTexture gl_TEXTURE_2D) .
  saveAndRestoreUnless (==0) gl_ELEMENT_ARRAY_BUFFER_BINDING (glBindBuffer gl_ELEMENT_ARRAY_BUFFER_BINDING) .
  saveAndRestoreUnless (==0) gl_ARRAY_BUFFER_BINDING (glBindBuffer gl_ARRAY_BUFFER) $ do
    glBindVertexArray vao
    glUseProgram program -- assume that the uniform for the sampler is already set
    glActiveTexture gl_TEXTURE0
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ibo
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
  let !offsetBytes = fromIntegral $ chunkOffset * sizeOf (undefined :: QuadAttribs)
      !rangeBytes = (bufferLen - chunkOffset)   * sizeOf (undefined :: QuadAttribs)
      !invalidateBufferBit
        | chunkOffset == 0 = gl_MAP_INVALIDATE_BUFFER_BIT
        | otherwise        = 0
  glBindTexture gl_TEXTURE_2D chunkTexId
  !ptr <- glMapBufferRange gl_ARRAY_BUFFER offsetBytes (fromIntegral rangeBytes) $
          gl_MAP_WRITE_BIT            .|.
          gl_MAP_INVALIDATE_RANGE_BIT .|.
          gl_MAP_FLUSH_EXPLICIT_BIT   .|.
          gl_MAP_UNSYNCHRONIZED_BIT   .|.
          invalidateBufferBit
  !flushCount <- foldM (\off quad -> off + 1 <$ pokeElemOff ptr off quad) 0 chunkQuads
  glFlushMappedBufferRange gl_ARRAY_BUFFER 0 . fromIntegral $
    flushCount * sizeOf (undefined :: QuadAttribs)
  unmapSuccess <- glUnmapBuffer gl_ARRAY_BUFFER
  let !shouldDraw = unmapSuccess == fromIntegral gl_TRUE
  when shouldDraw $ glDrawElements gl_TRIANGLES (fromIntegral flushCount * 6) gl_UNSIGNED_INT (plusPtr nullPtr $ chunkOffset * sizeOf (undefined :: GLuint))
  return shouldDraw

vShader :: ByteString
vShader =
  "#version 110\n\

  \attribute vec2 position;\
  \attribute vec2 texPosition;\
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
  saveAndRestoreUnless (==0) gl_ELEMENT_ARRAY_BUFFER_BINDING (glBindBuffer gl_ELEMENT_ARRAY_BUFFER_BINDING) .
  saveAndRestore gl_CURRENT_PROGRAM glUseProgram $ do
    vao <- glGen glGenVertexArrays
    vbo <- glGen glGenBuffers
    ibo <- glGen glGenBuffers

    glBindVertexArray vao
    
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER bufferBytes nullPtr gl_STREAM_DRAW

    let indices = Vector.concat . zipWith (Vector.map . (+) . (4*)) [0..] . replicate bufferLen $ Vector.fromList [0,2,1,1,2,3] :: Vector.Vector GLuint

    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ibo
    Vector.unsafeWith indices $ \ptr ->
      glBufferData gl_ELEMENT_ARRAY_BUFFER
      (fromIntegral $ bufferLen * sizeOf (undefined :: GLuint) * 6) ptr
      gl_STATIC_DRAW

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

    return $! GraphicsState vbo ibo vao prog

draw :: GraphicsState -> [(Sprite, AffineTransform GLfloat)] -> IO Bool
draw !gs = drawChunks gs . chunksToDraw
