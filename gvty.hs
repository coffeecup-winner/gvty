import Data.IORef
import Graphics.UI.GLUT

import Gvty.Bindings
import Gvty.State

main :: IO ()
main = do
    (name, args) <- getArgsAndInitialize
    state <- newIORef newState
    s <- get state
    let size = stateWindowSize s
    initialWindowSize $= (\(w, h) -> Size w h) size
    initialDisplayMode $= [ DoubleBuffered ]
    window <- createWindow "gvty"
    reshapeCallback $= Just (onReshape state)
    depthFunc $= Just Less
    displayCallback $= onDisplay state
    idleCallback $= Just (onIdle state)
    keyboardMouseCallback $= Just (onInput state)
    mainLoop
