module Game.Graphics.Font where

import Graphics.Rendering.FTGL

data FontText = FontText
        { getString :: String
        , getFont :: Font
        }
