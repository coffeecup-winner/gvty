module Gvty.Graphics ( onDisplay ) where

import Control.Monad
import Data.IORef
import Graphics.UI.GLUT

import Gvty.State

onDisplay :: IORef State -> DisplayCallback
onDisplay state = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    --(x, y) <- get position
    s <- get state
    --translate $ Vector3 x y 0
    --rotate a $ Vector3 0 0.1 1
    --scale 0.7 0.7 (0.7 :: GLfloat)
    forM_ (statePoints s) $ \p ->
        preservingMatrix $ do
            let (x, y) = objPosition p
            color $ Color3 0.5 0.5 (0.5 :: GLfloat)
            translate $ Vector3 x y (0 :: GLfloat)
            cube 0.05
            color $ Color3 0 0 (0 :: GLfloat)
            cubeFrame 0.05
    swapBuffers

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

points ::Int -> [(GLfloat, GLfloat, GLfloat)]
points n = [(sin (2 * pi * k / n'), cos (2 * pi * k / n'), 0) | k <- [1..n']]
    where n' = fromIntegral n

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
