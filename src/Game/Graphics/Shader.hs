{-# LANGUAGE RankNTypes #-}
module Game.Graphics.Shader (compileShader, linkProgram) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSUnsafe
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31

compileShader :: BS.ByteString -> GLenum -> IO GLuint
compileShader src kind = do
  sid <- glCreateShader kind
  BSUnsafe.unsafeUseAsCStringLen src $ \(ptr, len) ->
    alloca $ \ptrptr ->
    alloca $ \lenptr -> do
      poke ptrptr (castPtr ptr)
      poke lenptr $ fromIntegral len
      glShaderSource sid 1 ptrptr lenptr
  glCompileShader sid
  return sid

linkProgram :: GLuint -> GLuint -> IO GLuint
linkProgram vsid fsid = do
  pid <- glCreateProgram
  glAttachShader pid vsid
  glAttachShader pid fsid
  glLinkProgram pid
  return pid
