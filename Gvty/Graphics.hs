module Gvty.Graphics ( onDisplay ) where

import Control.Lens
import Control.Monad
import Data.Bits ((.&.))
import Data.IORef
import Data.Maybe
import Graphics.UI.GLUT

import Gvty.GraphicsCache
import Gvty.World

onDisplay :: IORef World -> IORef GraphicsCache -> DisplayCallback
onDisplay world cache = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    w <- get world
    when (isJust $ w^.worldNewObjectCoords) $ do
        draw sphere cache (fromJust $ w^.worldNewObjectCoords) 0.025 (0.3, 0.3, 0.3) (return ())
        draw sphere cache (fromJust $ w^.worldNewObjectPreviewCoords) 0.01 (0.3, 0.3, 0.3) (return ())
    forM_ (w^.worldObjects) $ \o ->
        draw sphere cache (o^.objPosition) 0.03 (0.5, 0.5, 0.5) (return ())
    forM_ (w^.worldPlanets) $ \p ->
        draw sphere cache (p^.planetPosition) (p^.planetRadius) (0.7, 0.7, 0.7) $
            rotate ((0.1 *) . fromIntegral $ w^.worldTime) $ Vector3 0.5 0.5 (0.5 :: GLfloat)
    forM_ (w^.worldAnomalies) $ \a ->
        draw circle cache (a^.anomalyPosition) (a^.anomalyRadius) (0.2, 0.2, 0.2) (return ())
    swapBuffers

draw :: Real a => (IORef GraphicsCache -> IO ()) -> IORef GraphicsCache -> (a, a) -> a -> (a, a, a) -> IO() -> IO ()
draw obj cache coords radius (r, g, b) action = preservingMatrix $ do
    let (x, y) = over both realToFrac coords
    when (x > -2 && x < 2 && y > -2 && y < 2) $ do
        color $ Color3 (realToFrac r) (realToFrac g) (realToFrac b :: GLfloat)
        translate $ vector2 x y
        scale (realToFrac radius) (realToFrac radius) (realToFrac radius :: GLfloat)
        action
        obj cache

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

vector2 :: GLfloat -> GLfloat -> Vector3 GLfloat
vector2 x y = Vector3 x y (0 :: GLfloat)

circle :: IORef GraphicsCache -> IO ()
circle _ = renderPrimitive TriangleFan $ mapM_ vertex3f (circlePoints 32)

circlePoints :: Int -> [(GLfloat, GLfloat, GLfloat)]
circlePoints n = [(sin (2 * pi * k / n'), cos (2 * pi * k / n'), 0) | k <- [1..n']]
    where n' = fromIntegral n

sphere :: IORef GraphicsCache -> IO ()
sphere cache = renderPrimitive TriangleStrip $
    mapM_ vertex3f =<< memoized cache spheresCache (\(n, m, k) -> spherePoints n m k) (32, 0.2, 3)

spherePoints :: Int -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
spherePoints n m k = getPoints heightToX ++ getPoints heightToY ++ getPoints heightToZ
                   where getPoints setHeight = map (setHeight . toSphere) $ cubePoints n getHeight
                         getHeight s t h = h + m * noise (s * k) (t * k)

heightToX (x, y, z) = (z, y, x)
heightToY (x, y, z) = (x, z, y)
heightToZ (x, y, z) = (x, y, z)

toSphere :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat)
toSphere (s, t, h) = (x, y, z)
    where x = h * (s / w)
          y = h * (t / w)
          z = h * (1 / w)
          w = sqrt $ s ^ 2 + t ^ 2 + 1

cubePoints :: Int -> (GLfloat -> GLfloat -> GLfloat -> GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
cubePoints n f = concatMap (\(x, y) -> getRow [x, y]) $ zip range (tail range)
    where getRow row = [(s, t, f s t h) | s <- range, t <- row, h <- [-1, 1]]
          range = map (/ n') [-n'..n'] :: [GLfloat]
          n' = fromIntegral n

noise :: GLfloat -> GLfloat -> GLfloat
noise x y = nxy
    where i = floor x :: Int
          j = floor y :: Int
          x' = x - fromIntegral i
          y' = y - fromIntegral j
          ii = i .&. 255
          jj = j .&. 255
          gi00 = perm!!(ii + perm!!jj) `mod` 8
          gi01 = perm!!(ii + perm!!(jj + 1)) `mod` 8
          gi10 = perm!!(ii + 1 + perm!!jj) `mod` 8
          gi11 = perm!!(ii + 1 + perm!!(jj + 1)) `mod` 8
          n00 = dot (grad2!!gi00) (x', y')
          n01 = dot (grad2!!gi01) (x', y' - 1)
          n10 = dot (grad2!!gi10) (x' - 1, y')
          n11 = dot (grad2!!gi11) (x' - 1, y' - 1)
          u = fade x'
          v = fade y'
          nx00 = lerp n00 n10 u
          nx01 = lerp n01 n11 u
          nxy = lerp nx00 nx01 v

dot :: Real a => (a, a) -> (a, a) -> a
dot (a1, b1) (a2, b2) = a1 * a2 + b1 * b2

lerp :: Real a => a -> a -> a -> a
lerp a b t = (1 - t) * a + t * b

fade :: Num a => a -> a
fade t = t * t * t * (t * (t * 6 - 15) + 10)

grad2 = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)] :: [(GLfloat, GLfloat)]

perm = p ++ p

p = [151,160,137,91,90,15,
 131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
 190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
 88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
 77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
 102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
 135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
 5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
 223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
 129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
 251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
 49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
 138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180]
