import           Control.Applicative
import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Data.Maybe
import           Data.Time
import           Game.Graphics
import qualified Graphics.UI.GLFW    as GLFW
import           Linear.V2
import           Text.Printf         (printf)

windowWidth, windowHeight :: Num a => a
windowWidth  = 1024
windowHeight = 768

main :: IO ()
main = do
  glfwInitialized <- GLFW.init
  unless glfwInitialized $ error "failed to initialize GLFW"
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  maybeWindow <- GLFW.createWindow windowWidth windowHeight "Wizard!" Nothing Nothing
  let window = fromMaybe (error "failed to open window") maybeWindow
  GLFW.makeContextCurrent maybeWindow
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
    GLFW.swapBuffers window
  stop <- getCurrentTime
  let spf = realToFrac $ stop `diffUTCTime` start / frames
  putStrLn $ secs spf ++ "/frame"
  GLFW.terminate

secs :: Double -> String
secs k
  | k < 0      = '-' : secs (-k)
  | k >= 1e9   = (k/1e9)  `with` "Gs"
  | k >= 1e6   = (k/1e6)  `with` "Ms"
  | k >= 1e4   = (k/1e3)  `with` "Ks"
  | k >= 1     = k        `with` "s"
  | k >= 1e-3  = (k*1e3)  `with` "ms"
  | k >= 1e-6  = (k*1e6)  `with` "us"
  | k >= 1e-9  = (k*1e9)  `with` "ns"
  | k >= 1e-12 = (k*1e12) `with` "ps"
  | otherwise  = printf "%g s" k
  where t `with` u
          | t >= 1e9  = printf "%.4g %s" t u
          | t >= 1e6  = printf "%.0f %s" t u
          | t >= 1e5  = printf "%.0f %s" t u
          | t >= 1e4  = printf "%.0f %s" t u
          | t >= 1e3  = printf "%.0f %s" t u
          | t >= 1e2  = printf "%.0f %s" t u
          | t >= 1e1  = printf "%.1f %s" t u
          | otherwise = printf "%.2f %s" t u
