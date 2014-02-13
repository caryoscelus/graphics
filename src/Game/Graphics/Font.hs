module Game.Graphics.Font where

type Font = ()

data FontText = FontText
        { getString :: String
        , getFont :: Font
        }
