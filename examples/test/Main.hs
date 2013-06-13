import Control.Applicative
import Control.Monad
import Data.Colour
import Data.Colour.Names
import Data.Time
import Game.Graphics
import Graphics.UI.GLFW (DisplayOptions (..))
import Linear.V2
import qualified Graphics.UI.GLFW as GLFW

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
  GLFW.setWindowBufferSwapInterval 0
  graphicsState <- initializeGraphics
  tex <- either error id <$> loadTexture Nearest "examples/test/wizard/wizard.png"
  let spr1 = mapTransform fromIntegral $ sprite (V2 3 7) (V2 55 82) tex
      spr2 = mapTransform fromIntegral $ modulatedSprite ((yellowgreen :: Colour Double) `withOpacity` 0.75) (V2 2 100) (V2 52 80) tex
      image = do
        scale $ V2 (recip $ windowWidth / 2) (recip $ windowHeight / 2)
        msum . take 19250 $ cycle [spr1, translate (V2 (-20) 0) *> spr2]
  start <- getCurrentTime
  forM_ [0..999::Int] $ \_ -> do
    clear
    _ <- draw graphicsState image
    GLFW.swapBuffers
  stop <- getCurrentTime
  print $ stop `diffUTCTime` start / 1000
  GLFW.terminate
