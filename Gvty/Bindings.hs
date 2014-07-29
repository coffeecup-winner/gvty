module Gvty.Bindings ( onReshape
                     , onInput
                     , onDisplay
                     , onIdle
                     ) where

import Data.IORef
import Graphics.UI.GLUT

import Gvty.Graphics
import Gvty.State

onReshape :: IORef State -> ReshapeCallback
onReshape state (Size width height) = do
    viewport $= (Position 0 0, Size x x)
    state $~! setWindowSize x x
        where x = if width < height then width else height

onInput :: IORef State -> KeyboardMouseCallback
onInput state key Down _ (Position mx my) = case key of
    (MouseButton LeftButton) -> state $~! add mx my
    _ -> return ()
onInput state key Up _ (Position mx my) = case key of
    (MouseButton LeftButton) -> state $~! fire mx my
    _ -> return ()

onIdle :: IORef State -> IdleCallback
onIdle state = do
    time <- get elapsedTime
    state $~! move time
    postRedisplay Nothing

