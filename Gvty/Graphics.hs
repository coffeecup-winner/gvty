module Gvty.Graphics ( onDisplay ) where

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Maybe
import Graphics.UI.GLUT

import Gvty.World

onDisplay :: IORef World -> DisplayCallback
onDisplay world = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    w <- get world
    when (isJust $ w^.worldNewObjectCoords) $ do
        drawCircle (fromJust $ w^.worldNewObjectCoords) 0.025 (0.3, 0.3, 0.3)
        drawCircle (fromJust $ w^.worldNewObjectPreviewCoords) 0.01 (0.3, 0.3, 0.3)
    forM_ (w^.worldObjects) $ \o ->
        drawCircle (o^.objPosition) 0.03 (0.5, 0.5, 0.5)
    forM_ (w^.worldPlanets) $ \p ->
        drawCircle (p^.planetPosition) (p^.planetRadius) (0.7, 0.7, 0.7)
    forM_ (w^.worldAnomalies) $ \a ->
        drawCircle (a^.anomalyPosition) (a^.anomalyRadius) (0.2, 0.2, 0.2)
    swapBuffers

drawCircle :: Real a => (a, a) -> a -> (a, a, a) -> IO ()
drawCircle coords radius (r, g, b) = preservingMatrix $ do
    let (x, y) = over both realToFrac coords
    color $ Color3 (realToFrac r) (realToFrac g) (realToFrac b :: GLfloat)
    translate $ vector2 x y
    circle $ realToFrac radius

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

vector2 :: GLfloat -> GLfloat -> Vector3 GLfloat
vector2 x y = Vector3 x y (0 :: GLfloat)

circle :: GLfloat -> IO ()
circle r = renderPrimitive TriangleFan $ mapM_ vertex3f (points r 32)

points :: GLfloat -> Int -> [(GLfloat, GLfloat, GLfloat)]
points r n = [(r * sin (2 * pi * k / n'), r * cos (2 * pi * k / n'), 0) | k <- [1..n']]
    where n' = fromIntegral n
