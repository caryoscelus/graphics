{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Graphics.Triangles (GraphicsState (), Triangles (), draw, applyTransform, sprite, initializeGraphics) where

import Control.Applicative
import Control.Lens (traverse, toListOf, maximumOf, minimumOf)
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString, useAsCString)
import Data.Colour as Colour
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import Data.Maybe
import Data.List
import Data.Vector.Storable (Vector)
import Foreign.Ptr
import Foreign.Storable
import Game.Graphics.AffineTransform (AffineTransform)
import Game.Graphics.Attributes hiding (applyTransform)
import Game.Graphics.Shader
import Game.Graphics.Texture
import Game.Graphics.Utils
import Graphics.Rendering.OpenGL.Raw.Core32
import Linear.V2
import Linear.V3

import qualified Data.Vector.Storable as Vector
import qualified Game.Graphics.Attributes as Attributes

data Triangles =
  Triangles { triTexId      :: !GLuint
            , triIndices    :: !(Vector GLuint)
            , triAttributes :: !(Vector Attributes)
            } deriving Show

-- TODO This function is entered a lot, consuming a lot of CPU
-- time. Optimize or use less somehow.
applyTransform :: AffineTransform GLfloat -> Triangles -> Triangles
{-# INLINE applyTransform #-}
applyTransform trans tris =
  tris { triAttributes = Vector.map (Attributes.applyTransform trans) $
                         triAttributes tris }

vectorBytes :: forall a. Storable a => Vector a -> Int
vectorBytes = (* sizeOf (undefined :: a)) . Vector.length

indexBytes :: Triangles -> Int
indexBytes = vectorBytes . triIndices

attributeBytes :: Triangles -> Int
attributeBytes = vectorBytes . triAttributes

inferIndices :: Eq a => [a] -> ([a], [Int])
inferIndices xs =
  let xs' = nub xs
      ixs = catMaybes $ map (`elemIndex` xs') xs
  in (xs', ixs)

-- TODO Add a parameter for the origin in texture coordinates instead
-- of just guessing from the height and width of the shape.

-- @triangles texture color trianglesInTexturePixelCoords@
triangles :: Real a => Texture -> AlphaColour a -> [V3 (V2 Int)] -> Triangles
triangles tex _ [] = Triangles (texId tex) Vector.empty Vector.empty
triangles tex (alphaColourConvert -> color) tris =
  Triangles (texId tex) (Vector.map fromIntegral $ Vector.fromList ixs) attrs
  where (!verts, !ixs) = inferIndices $ toListOf (traverse.traverse) tris
        !minX = fromJust $ minimumOf (traverse._x) verts
        !maxX = fromJust $ maximumOf (traverse._x) verts
        !minY = fromJust $ minimumOf (traverse._y) verts
        !maxY = fromJust $ maximumOf (traverse._y) verts
        !offsetX = minX + (maxX-minX) `div` 2
        !offsetY = minY + (maxY-minY) `div` 2
        !alpha = alphaChannel color
        RGB !red !green !blue = toRGB $ color `Colour.over` black
        mkAttrs (V2 texX texY) =
          Attributes
          (fromIntegral $ texX - offsetX) (fromIntegral . negate $ texY - offsetY)
          (fromIntegral texX / texW) (fromIntegral texY / texH)
          red green blue alpha
        !attrs = Vector.fromList $ map mkAttrs verts
        V2 !texW !texH = fromIntegral <$> texSize tex

-- | @sprite texture color upperLeft dimensions@
sprite :: Real a => Texture -> AlphaColour a -> V2 Int -> V2 Int -> Triangles
sprite tex color (V2 x y) (V2 w h) =
  triangles tex color [ V3 (V2 l t) (V2 l b) (V2 r t)
                      , V3 (V2 r t) (V2 l b) (V2 r b)
                      ]
  where !t = y
        !r = x + w - 1
        !b = y + h - 1
        !l = x

bufferBytes :: Num a => a
bufferBytes = 4*1024*1024

drawTriangles :: Int -> Int -> Triangles -> IO Bool
drawTriangles elemOffset arrayOffset Triangles{..} = do
  let !elemOffsetBytes = elemOffset * sizeOf (undefined :: GLuint)
  maybeElemCount <- writeData gl_ELEMENT_ARRAY_BUFFER elemOffsetBytes                                  triIndices
  maybeAttrCount <- writeData gl_ARRAY_BUFFER         (arrayOffset * sizeOf (undefined :: Attributes)) triAttributes
  case (maybeElemCount, maybeAttrCount) of
    (Just !elemCount, Just _attrCount) -> do
      glBindTexture gl_TEXTURE_2D triTexId
      glDrawElements gl_TRIANGLES (fromIntegral elemCount) gl_UNSIGNED_INT . intPtrToPtr $ fromIntegral elemOffsetBytes
      return True
    _ -> return False

writeData :: forall a. (Show a, Storable a) => GLenum -> Int -> Vector a -> IO (Maybe Int)
writeData target offsetBytes xs = do
  let !rangeBytes = bufferBytes - offsetBytes
      !invalidateBufferBit
        | offsetBytes == 0 = gl_MAP_INVALIDATE_BUFFER_BIT
        | otherwise        = 0
  ptr <- glMapBufferRange target (fromIntegral offsetBytes) (fromIntegral rangeBytes) $
         gl_MAP_WRITE_BIT            .|.
         gl_MAP_INVALIDATE_RANGE_BIT .|.
         gl_MAP_FLUSH_EXPLICIT_BIT   .|.
         gl_MAP_UNSYNCHRONIZED_BIT   .|.
         invalidateBufferBit
  !count <- Vector.foldM' (\ix x -> ix+1 <$ pokeElemOff ptr ix x) 0 xs
  glFlushMappedBufferRange target 0 . fromIntegral $ count * sizeOf (undefined :: a)
  success <- glUnmapBuffer target
  return $! if success == fromIntegral gl_TRUE then Just count else Nothing

chunksToDraw :: [Triangles] -> [(Int, Int, Triangles)]
chunksToDraw []     = []
chunksToDraw (x:xs) =
  map (regroup . unzip3) $
  groupBy (\(_, _, !t) (!elemOff, !attrOff, !u) -> triTexId t == triTexId u && elemOff /= 0 && attrOff /= 0) $
  scanl accumOffsets (0, 0, x) xs
  where accumOffsets (!prevElemOff, !prevAttrOff, !prev) t =
          let !prevElemCount = Vector.length $ triIndices prev
              !prevAttrCount = Vector.length $ triAttributes prev
              !elemOff' = prevElemOff + prevElemCount
              !attrOff' = prevAttrOff + prevAttrCount
              !elemOff | elemOff' + indexBytes t > bufferBytes = 0
                       | otherwise = elemOff'
              !attrOff | attrOff' + attributeBytes t > bufferBytes = 0
                       | otherwise = attrOff'
          in (elemOff, attrOff, offsetElems attrOff t)
        -- TODO regroup is a major allocator. optimize this somehow
        {-# INLINE regroup #-}
        regroup (!elemOff:_, !attrOff:_, ts@(t:_)) =
          (elemOff, attrOff, Triangles (triTexId t)
                             (Vector.concat $ triIndices <$> ts)
                             (Vector.concat $ triAttributes <$> ts))
        regroup _ = error "BUG in chunksToDraw"
        offsetElems (fromIntegral -> !off) tri = tri { triIndices = Vector.map (+off) $ triIndices tri }

data GraphicsState =
  GraphicsState { vbo     :: !GLuint
                , ibo     :: !GLuint
                , vao     :: !GLuint
                , program :: !GLuint
                }

draw :: GraphicsState -> [Triangles] -> IO Bool
draw GraphicsState{..} tris =
  saveAndRestore gl_VERTEX_ARRAY_BINDING glBindVertexArray .
  saveAndRestore gl_CURRENT_PROGRAM glUseProgram .
  saveAndRestore gl_ACTIVE_TEXTURE glActiveTexture .
  saveAndRestore gl_TEXTURE_2D (glBindTexture gl_TEXTURE_2D) .
  saveAndRestoreUnless (==0) gl_ELEMENT_ARRAY_BUFFER_BINDING (glBindBuffer gl_ELEMENT_ARRAY_BUFFER_BINDING) .
  saveAndRestoreUnless (==0) gl_ARRAY_BUFFER_BINDING (glBindBuffer gl_ARRAY_BUFFER) .
  saveAndRestore4 gl_BLEND_SRC_RGB gl_BLEND_DST_RGB gl_BLEND_SRC_ALPHA gl_BLEND_DST_ALPHA glBlendFuncSeparate $ do
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

    !drewCleanly <- foldM (\ !success (elemOff, attrOff, ts) ->
                            (&&success) <$> drawTriangles elemOff attrOff ts)
                    True $ chunksToDraw tris

    unless srgbWasEnabled $ glDisable gl_FRAMEBUFFER_SRGB  
    return drewCleanly

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

    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ibo
    glBufferData gl_ELEMENT_ARRAY_BUFFER bufferBytes nullPtr gl_STREAM_DRAW

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
      (fromIntegral $ sizeOf (undefined :: Attributes)) $ intPtrToPtr 0
    glVertexAttribPointer vTexPosition 2 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attributes)) . intPtrToPtr .
      fromIntegral $ sizeOf (undefined :: V2 GLfloat)
    glVertexAttribPointer vModulateColor 4 gl_FLOAT (fromIntegral gl_FALSE)
      (fromIntegral $ sizeOf (undefined :: Attributes)) . intPtrToPtr .
      fromIntegral $ sizeOf (undefined :: V2 GLfloat) * 2

    glEnableVertexAttribArray vPosition
    glEnableVertexAttribArray vTexPosition
    glEnableVertexAttribArray vModulateColor

    return $! GraphicsState vbo ibo vao prog
