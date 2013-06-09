{-# OPTIONS -fexpose-all-unfoldings #-}
module Game.Graphics.Utils (ptrResult, glGen, glGet, saveAndRestore, saveAndRestoreUnless) where

import Control.Monad
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31

ptrResult :: Storable a => (Ptr a -> IO ()) -> IO a
ptrResult f = alloca $ \ptr -> f ptr >> peek ptr

glGen :: (Num a, Storable b) => (a -> Ptr b -> IO ()) -> IO b
glGen = ptrResult . ($1)

glGet :: Num a => GLenum -> IO a
glGet = fmap fromIntegral . ptrResult . glGetIntegerv

saveAndRestore :: GLenum -> (GLuint -> IO ()) -> IO c -> IO c
saveAndRestore = saveAndRestoreUnless $ const False

saveAndRestoreUnless :: (GLuint -> Bool) -> GLenum -> (GLuint -> IO ()) -> IO b -> IO b
saveAndRestoreUnless p target f m = do
  old <- glGet target
  x <- m
  unless (p old) $ f old
  return x
  
