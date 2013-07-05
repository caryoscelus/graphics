import           Control.Applicative
import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Game.Graphics
import           Graphics.UI.GLFW    (DisplayOptions (..))
import qualified Graphics.UI.GLFW    as GLFW
import           Linear.V2

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
  GLFW.setWindowTitle "Wizard!"
  GLFW.setWindowBufferSwapInterval 0
  glViewport 0 0 windowWidth windowHeight
  graphicsState <- initializeGraphics
  tex <- either error id <$> loadTexture Linear "examples/example1/wizard/wizard.png"
  let spr1 = sprite (V2 3 7) (V2 55 82) tex
      spr2 = modulatedSprite ((yellowgreen :: Colour Double) `withOpacity` 0.5) (V2 2 100) (V2 52 80) tex
      image n = do
        scale $ V2 (recip $ windowWidth / 2) (recip $ windowHeight / 2)
        msum $ map (\x -> scale (let y = (x+1)/75 in V2 y y) *> translate (V2 (x*5 - 250) 0) *>
                          ((translate (V2 0 (sin (pi*x/50 + n/250) * 250)) *> spr1) <|>
                           (translate (V2 0 (cos (pi*x/50 + n/250) * 250)) *> spr2))) [0..99]
  forM_ [0..9999] $ \x -> do
    clear
    _ <- draw graphicsState $ image x
    GLFW.swapBuffers
  GLFW.terminate
