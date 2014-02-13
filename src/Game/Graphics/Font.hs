{-# LANGUAGE RecordWildCards #-}

module Game.Graphics.Font
       ( FontText(..)
       , Font, loadFont
       , drawFontText
       ) where

import Graphics.Rendering.FTGL

import Game.Graphics.AffineTransform        (AffineTransform)

data FontText = FontText
        { getFont :: Font
        , getString :: String
        }

loadFont :: String -> IO Font
loadFont = createPixmapFont

drawFontText :: AffineTransform -> FontText -> IO Bool
drawFontText _ FontText{..} = do
    setFontFaceSize getFont 24 72
    renderFont getFont getString Front
    return True
