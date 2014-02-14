{-# LANGUAGE RecordWildCards #-}

module Game.Graphics.Font
       ( FontText(..)
       , Font, loadFont
       , drawFontText
       ) where

import Graphics.Rendering.FTGL

import Game.Graphics.AffineTransform (AffineTransform, withTransformRasterGl)

data FontText = FontText
        { getFont :: Font
        , getString :: String
        }

loadFont :: String -> IO Font
loadFont = createPixmapFont

drawFontText :: AffineTransform -> FontText -> IO Bool
drawFontText trans FontText{..} = do
    setFontFaceSize getFont 24 72
    withTransformRasterGl trans $
        renderFont getFont getString Front
    return True
