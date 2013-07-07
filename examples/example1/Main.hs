import           Control.Applicative
import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Data.Time
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
  tex <- either error id <$> loadTexture Standard Linear "examples/example1/wizard/wizard.png"
  let spr1 = sprite (V2 3 7) (V2 55 82) tex
      spr2 = modulatedSprite (yellowgreen `withOpacity` 0.5) (V2 2 100) (V2 52 80) tex
      image n = do
        scale $ V2 (recip $ windowWidth / 2) (recip $ windowHeight / 2)
        msum $ map (\x -> scale (let y = (x+1)/75 in V2 y y) *> translate (V2 (x*5 - 250) 0) *>
                          ((translate (V2 0 (sin (pi*x/50 + n/250) * 250)) *> spr1) <|>
                           (translate (V2 0 (cos (pi*x/50 + n/250) * 250)) *> spr2))) [0..99]
  let frames :: Num a => a
      frames = 10000
  start <- getCurrentTime
  forM_ [1..frames] $ \x -> do
    clear
    _ <- draw graphicsState $ image x
    GLFW.swapBuffers
  stop <- getCurrentTime
  let spf = realToFrac $ stop `diffUTCTime` start / frames :: Double
  putStrLn $ show (round $ spf * 1000000 :: Int) ++ " us/frame, " ++ show (round $ recip spf :: Int) ++ " frames/s, " ++ show (round $ recip spf * 200 :: Int) ++ " sprites/s"
  GLFW.terminate
