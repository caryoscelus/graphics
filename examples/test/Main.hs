import Control.Applicative
import Control.Monad
import Game.Graphics
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (DisplayOptions (..))

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
                             }

main :: IO ()
main = do
  glfwInitialized <- GLFW.initialize
  unless glfwInitialized $ error "failed to initialize GLFW"
  windowOpened <- GLFW.openWindow displayOptions
  unless windowOpened $ error "failed to open window"
  graphicsState <- initializeGraphics
  tex <- either error id <$> loadTexture "examples/test/wizard/wizard.png"
  let spr = sprite 0 60 80 0 tex
  draw graphicsState $ return spr
  threadDelay 1000000
  GLFW.terminate