import Control.Applicative
import Control.Monad
import Game.Graphics
import Graphics.UI.GLFW (DisplayOptions (..))
import Linear.V2
import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent

displayOptions :: DisplayOptions
displayOptions =
  GLFW.defaultDisplayOptions { displayOptions_width                   = 1024
                             , displayOptions_height                  = 768
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
  let spr1 = sprite 0 59 89 0 tex
      spr2 = sprite 90 59 179 0 tex
  draw graphicsState $ do
    translate (V2 0 (-1))
    spr1 <$ translate (V2 (-1) 0) <|> return spr2
  GLFW.swapBuffers
  threadDelay 10000000
  GLFW.terminate
