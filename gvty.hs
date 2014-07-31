import Control.Lens
import Data.IORef
import Graphics.UI.GLUT

import Gvty.Bindings
import Gvty.World

main :: IO ()
main = do
    (name, args) <- getArgsAndInitialize
    world <- newIORef newWorld
    s <- get world
    let size = s^.worldWindowSize
    initialWindowSize $= (\(w, h) -> Size w h) size
    initialDisplayMode $= [ DoubleBuffered ]
    window <- createWindow "gvty"
    reshapeCallback $= Just (onReshape world)
    depthFunc $= Just Less
    displayCallback $= onDisplay world
    idleCallback $= Just (onIdle world)
    keyboardMouseCallback $= Just (onInput world)
    mainLoop
