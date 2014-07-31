module Gvty.Graphics ( onDisplay ) where

import Control.Lens
import Control.Monad
import Data.IORef
import Graphics.UI.GLUT

import Gvty.World

onDisplay :: IORef World -> DisplayCallback
onDisplay world = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    s <- get world
    forM_ (s^.worldObjects) $ \o ->
        preservingMatrix $ do
            let (x, y) = o^.objPosition
            color $ Color3 0.5 0.5 (0.5 :: GLfloat)
            translate $ Vector3 x y (0 :: GLfloat)
            cube 0.05
            color $ Color3 0 0 (0 :: GLfloat)
            cubeFrame 0.05
    swapBuffers

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO ()
cube w = do
    renderPrimitive Quads $ mapM_ vertex3f
        [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
          ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
          ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
          (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
          ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
          ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]
