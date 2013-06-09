import Control.Applicative
import Control.Monad
import Game.Graphics
import Graphics.UI.GLFW (DisplayOptions (..))
import Linear.V2
import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent

windowWidth, windowHeight :: Num a => a
windowWidth  = 1024
windowHeight = 768

displayOptions :: DisplayOptions
displayOptions =
  GLFW.defaultDisplayOptions { displayOptions_width                   = windowWidth
                             , displayOptions_height                  = windowHeight
                             , displayOptions_numRedBits              = 8
                             , displayOptions_numGreenBits            = 8
                             , displayOptions_numBlueBits             = 8
                             , displayOptions_windowIsResizable       = False
                             , displayOptions_openGLVersion           = (2,1)
                             , displayOptions_openGLDebugContext      = True
                             }

main :: IO ()
main = do
  glfwInitialized <- GLFW.initialize
  unless glfwInitialized $ error "failed to initialize GLFW"
  windowOpened <- GLFW.openWindow displayOptions
  unless windowOpened $ error "failed to open window"
  GLFW.setWindowTitle "Foo"
  graphicsState <- initializeGraphics
  tex <- either error id <$> loadTexture "examples/test/wizard/wizard.png"
  let spr1 = mapTransform fromIntegral $ sprite 0 59 89 0 tex
      spr2 = mapTransform fromIntegral $ sprite 90 59 179 0 tex
  draw graphicsState $ do
    translate (V2 (-1) (-1))
    scale (V2 (recip $ windowWidth / 2) (recip $ windowHeight / 2))
    spr1 <|> translate (V2 50 0) *> spr2
  GLFW.swapBuffers
  threadDelay 10000000
  GLFW.terminate
