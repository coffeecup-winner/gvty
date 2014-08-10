module Gvty.Graphics ( onDisplay ) where

import Control.Lens
import Control.Monad
import Data.Bits ((.&.))
import Data.IORef
import Data.Maybe
import Graphics.UI.GLUT
import Data.Vect.Float as V
import Data.Vect.Float.OpenGL

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

draw :: Real a => (IORef GraphicsCache -> IO ()) -> IORef GraphicsCache -> Vec2 -> a -> (a, a, a) -> IO() -> IO ()
draw obj cache coords radius (r, g, b) action = preservingMatrix $
    when (len coords < 2) $ do
        color $ Color3 (realToFrac r) (realToFrac g) (realToFrac b :: GLfloat)
        glTranslate $ extendZero coords
        scale (realToFrac radius) (realToFrac radius) (realToFrac radius :: GLfloat)
        action
        obj cache

circle :: IORef GraphicsCache -> IO ()
circle _ = renderPrimitive TriangleFan $ mapM_ vertex (circlePoints 32)

circlePoints :: Int -> [Vec3]
circlePoints n = [Vec3 (sin $ 2 * pi * k / n') (cos $ 2 * pi * k / n') 0 | k <- [1..n']]
    where n' = fromIntegral n

sphere :: IORef GraphicsCache -> IO ()
sphere cache = renderPrimitive TriangleStrip $
    mapM_ vertex =<< memoized cache spheresCache (\(n, m, k) -> spherePoints n m k) (32, 0.2, 3)

spherePoints :: Int -> Float -> Float -> [Vec3]
spherePoints n m k = getPoints heightToX ++ getPoints heightToY ++ getPoints heightToZ
    where getPoints setHeight = map (setHeight . toSphere . modifyHeight) $ cubePoints n
          modifyHeight v@(Vec3 s t _) = v &+ Vec3 0 0 (getNoise s t)
          getNoise s t = m * noise (k *& Vec2 s t)
          heightToX (Vec3 x y z) = Vec3 z y x
          heightToY (Vec3 x y z) = Vec3 x z y
          heightToZ (Vec3 x y z) = Vec3 x y z

toSphere :: Vec3 -> Vec3
toSphere (Vec3 s t h) = mkVec3 (x, y, z)
    where x = h * (s / w)
          y = h * (t / w)
          z = h * (1 / w)
          w = sqrt $ s ^ 2 + t ^ 2 + 1

cubePoints :: Int -> [Vec3]
cubePoints n = concatMap (\(x, y) -> getRow [x, y]) $ zip range (tail range)
    where getRow row = [Vec3 s t h | s <- range, t <- row, h <- [-1, 1]]
          range = map (/ n') [-n'..n'] :: [Float]
          n' = fromIntegral n

noise :: Vec2 -> Float
noise vec = nxy
    where idx@(i, j) = mapVec' floor vec
          vec' = vec &- mkVec2 (over both fromIntegral idx)
          (ii, jj) = over both (.&. 255) idx
          g00 = getGrad  ii       jj
          g10 = getGrad (ii + 1)  jj
          g01 = getGrad  ii      (jj + 1)
          g11 = getGrad (ii + 1) (jj + 1)
          n00 = g00 &. (vec' &+ Vec2   0    0)
          n10 = g10 &. (vec' &+ Vec2 (-1)   0)
          n01 = g01 &. (vec' &+ Vec2   0  (-1))
          n11 = g11 &. (vec' &+ Vec2 (-1) (-1))
          n = Mat2
                (Vec2 n00 n10)
                (Vec2 n01 n11)
          (u, v) = mapVec' fade vec'
          nxy = (n *. lerpVec u) &. lerpVec v
          getGrad i j = grad2 !! (perm !! (i + perm !! j) `mod` 8)

mapVec' :: (Float -> a) -> Vec2 -> (a, a)
mapVec' f (Vec2 x y) = (f x, f y)

lerpVec :: Float -> Vec2
lerpVec t = Vec2 (1 - t) t

fade :: Float -> Float
fade t = t * t * t * (t * (t * 6 - 15) + 10)

grad2 = map mkVec2 [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

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
