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
            let (x, y) = over both realToFrac $ o^.objPosition
            color $ Color3 0.5 0.5 (0.5 :: GLfloat)
            translate $ vector2 x y
            circle 0.05
    forM_ (s^.worldPlanets) $ \p ->
        preservingMatrix $ do
            let (x, y) = over both realToFrac $ p^.planetPosition
                r = p^.planetRadius
            color $ Color3 0.7 0.7 (0.7 :: GLfloat)
            translate $ vector2 x y
            circle $ realToFrac r
    swapBuffers

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

vector2 :: GLfloat -> GLfloat -> Vector3 GLfloat
vector2 x y = Vector3 x y (0 :: GLfloat)

circle :: GLfloat -> IO ()
circle r = renderPrimitive TriangleFan $ mapM_ vertex3f (points r 32)

points :: GLfloat -> Int -> [(GLfloat, GLfloat, GLfloat)]
points r n = [(r * sin (2 * pi * k / n'), r * cos (2 * pi * k / n'), 0) | k <- [1..n']]
    where n' = fromIntegral n
