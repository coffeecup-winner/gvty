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
    let (w, h) = over both fromIntegral $ s^.worldWindowSize
    initialWindowSize $= Size w h
    initialDisplayMode $= [ DoubleBuffered ]
    window <- createWindow "gvty"
    reshapeCallback $= Just (onReshape world)
    depthFunc $= Just Less
    displayCallback $= onDisplay world
    idleCallback $= Just (onIdle world)
    keyboardMouseCallback $= Just (onInput world)
    mainLoop
