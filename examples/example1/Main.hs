import           Control.Applicative
import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Data.Maybe
import           Data.Time
import           Game.Graphics
import qualified Graphics.UI.GLFW    as GLFW
import           Linear.V2

windowWidth, windowHeight :: Num a => a
windowWidth  = 1024
windowHeight = 768

main :: IO ()
main = do
  GLFW.setErrorCallback . Just $ \e str -> putStrLn $ show e ++ ": " ++ str
  glfwInitialized <- GLFW.init
  unless glfwInitialized $ error "failed to initialize GLFW"
  mapM_ GLFW.windowHint [ GLFW.WindowHint'Resizable False
                        , GLFW.WindowHint'AlphaBits 0
                        , GLFW.WindowHint'DepthBits 0
                        , GLFW.WindowHint'StencilBits 0
                        , GLFW.WindowHint'sRGBCapable True
                        , GLFW.WindowHint'ContextVersionMajor 2
                        , GLFW.WindowHint'ContextVersionMinor 1
                        ]
  window <- fromMaybe (error "failed to create window") <$>
            GLFW.createWindow windowWidth windowHeight "Wizard!" Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  GLFW.swapInterval 0
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
    return ()
    GLFW.swapBuffers window
  stop <- getCurrentTime
  let spf = realToFrac $ stop `diffUTCTime` start / frames :: Double
  putStrLn $ show (round $ spf * 1000000 :: Int) ++ " us/frame"
  freeTexture tex
  freeGraphics graphicsState
  GLFW.destroyWindow window
  GLFW.terminate
