import Control.Lens
import Control.Monad hiding (forM_, mapM_)
import Control.Monad.State (execState)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable (forM_, mapM_)
import Data.IORef
import Data.Maybe
import Graphics.UI.GLUT
import Prelude hiding (mapM_)

import Gvty.Bindings
import Gvty.Serialization
import Gvty.World

main :: IO ()
main = do
    (name, args) <- getArgsAndInitialize
    let filename = listToMaybe args
    world <- newIORef newWorld
    forM_ filename $ \file -> do
        json <- L.readFile $ fromJust filename
        let w = decode json :: Maybe World
        mapM_ (world $=) w
    s <- get world
    let (w, h) = over both fromIntegral $ s^.worldWindowSize
    initialWindowSize $= Size w h
    initialDisplayMode $= [ DoubleBuffered ]
    window <- createWindow "gvty"
    reshapeCallback $= Just (onReshape world)
    depthFunc $= Just Less
    displayCallback $= onDisplay world
    idleCallback $= Just (onIdle world)
    motionCallback $= Just (onMotion world)
    keyboardMouseCallback $= Just (onInput world)
    mainLoop
