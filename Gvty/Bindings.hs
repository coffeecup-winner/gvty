module Gvty.Bindings ( onReshape
                     , onInput
                     , onDisplay
                     , onIdle
                     ) where

import Control.Lens
import Control.Monad.State (execState)
import Data.IORef
import Graphics.UI.GLUT

import Gvty.Graphics
import Gvty.World

onReshape :: IORef World -> ReshapeCallback
onReshape world (Size width height) = do
    viewport $= (Position 0 0, Size x x)
    world $~! (worldWindowSize .~ (x, x))
        where x = if width < height then width else height

onInput :: IORef World -> KeyboardMouseCallback
onInput world key Down _ (Position mx my) = case key of
    (MouseButton LeftButton) -> world $~! execState (add mx my)
    _ -> return ()
onInput world key Up _ (Position mx my) = case key of
    (MouseButton LeftButton) -> world $~! execState (fire mx my)
    _ -> return ()

onIdle :: IORef World -> IdleCallback
onIdle world = do
    time <- get elapsedTime
    world $~! execState (move time)
    postRedisplay Nothing

