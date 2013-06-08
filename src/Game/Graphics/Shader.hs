module Game.Graphics.Shader (compileShader, linkProgram) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSUnsafe
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31
import Game.Graphics.Utils

-- TODO when there are errors just throw them as exceptions

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
  len <- ptrResult $ glGetShaderiv sid gl_INFO_LOG_LENGTH
  infoLog <- allocaArray (fromIntegral len) $ \ptr -> do
               glGetShaderInfoLog sid (fromIntegral len) nullPtr ptr
               BSUnsafe.unsafePackCStringLen (castPtr ptr, fromIntegral len)
  BSC.putStrLn infoLog
  return sid

linkProgram :: GLuint -> GLuint -> IO GLuint
linkProgram vsid fsid = do
  pid <- glCreateProgram
  glAttachShader pid vsid
  glAttachShader pid fsid
  glLinkProgram pid
  len <- ptrResult $ glGetShaderiv pid gl_INFO_LOG_LENGTH
  infoLog <- allocaArray (fromIntegral len) $ \ptr -> do
               glGetProgramInfoLog pid (fromIntegral len) nullPtr ptr
               BSUnsafe.unsafePackCStringLen (castPtr ptr, fromIntegral len)
  BSC.putStrLn infoLog
  return pid
