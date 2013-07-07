{-# OPTIONS -fexpose-all-unfoldings #-}
module Game.Graphics.Utils (ptrResult, glGen, glGet) where

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
