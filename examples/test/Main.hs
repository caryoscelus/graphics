import Control.Applicative
import Control.Monad
import Data.Colour
import Data.Colour.Names
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
  GLFW.setWindowTitle "Wizard!"
  graphicsState <- initializeGraphics
  tex <- either error id <$> loadTexture Linear "examples/test/wizard/wizard.png"
  tex2 <- either error id <$> loadTexture Nearest "examples/test/wizard/wizard.png"
  let spr1 = mapTransform fromIntegral $ sprite (V2 3 7) (V2 55 82) tex
      spr2 = mapTransform fromIntegral $ modulatedSprite ((yellowgreen :: Colour Double) `withOpacity` 0.5) (V2 2 100) (V2 52 80) tex
      spr3 = mapTransform fromIntegral $ sprite (V2 3 7) (V2 55 82) tex2
      spr4 = mapTransform fromIntegral $ modulatedSprite ((yellowgreen :: Colour Double) `withOpacity` 0.5) (V2 2 100) (V2 52 80) tex2
      image x = do
        scale $ V2 (recip $ windowWidth / 2) (recip $ windowHeight / 2)
        let scaleOf1 = x / 500 + 0.25
        return () <|> (translate (V2 (x/100) (x-500)) *> rotate (-x*pi/1000))
        (scale (V2 scaleOf1 scaleOf1) *> spr1) <|> (translate (V2 (500-x) 0) *> rotate (x*pi/500) *> scale 3 *> spr2) <|>
          (translate (V2 0 200) *> ((scale (V2 scaleOf1 scaleOf1) *> spr3) <|> (translate (V2 (500-x) 0) *> rotate (x*pi/500) *> scale 3 *> spr4)))
  forM_ [0..999] $ \x -> do
    clear
    _ <- draw graphicsState $ image x
    GLFW.swapBuffers
  GLFW.terminate
