{-# OPTIONS -fexpose-all-unfoldings #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Game.Graphics.Attributes where

import Control.Applicative
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31
import Game.Graphics.AffineTransform
import Linear.V2

-- Attributes for a single vertex
data Attributes =
  Attributes { attribsPosX      :: !GLfloat
             , attribsPosY      :: !GLfloat
             , attribsTexX      :: !GLfloat
             , attribsTexY      :: !GLfloat
             , attribsModulateR :: !GLfloat
             , attribsModulateG :: !GLfloat
             , attribsModulateB :: !GLfloat
             , attribsModulateA :: !GLfloat
             } deriving Show

instance Storable Attributes where
  sizeOf    _ = sizeOf (undefined :: GLfloat) * 8
  alignment _ = alignment (undefined :: GLfloat)
  peek (castPtr -> !ptr) =
    Attributes        <$>
    peek        ptr   <*>
    peekElemOff ptr 1 <*>
    peekElemOff ptr 2 <*>
    peekElemOff ptr 3 <*>
    peekElemOff ptr 4 <*>
    peekElemOff ptr 5 <*>
    peekElemOff ptr 6 <*>
    peekElemOff ptr 7
  poke (castPtr -> !ptr) Attributes{..} = do
    poke        ptr   attribsPosX
    pokeElemOff ptr 1 attribsPosY
    pokeElemOff ptr 2 attribsTexX
    pokeElemOff ptr 3 attribsTexY
    pokeElemOff ptr 4 attribsModulateR
    pokeElemOff ptr 5 attribsModulateG
    pokeElemOff ptr 6 attribsModulateB
    pokeElemOff ptr 7 attribsModulateA

applyTransform :: AffineTransform -> Attributes -> Attributes
applyTransform t attrs = attrs { attribsPosX = x, attribsPosY = y }
  where V2 x y = apply t $ V2 (attribsPosX attrs) (attribsPosY attrs)
