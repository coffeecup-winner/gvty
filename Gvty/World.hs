{-# LANGUAGE TemplateHaskell #-}
module Gvty.World ( Obj(..)
                  , objPosition
                  , objVelocity
                  , Planet(..)
                  , planetPosition
                  , planetRadius
                  , Anomaly(..)
                  , anomalyPosition
                  , anomalyRadius
                  , World(..)
                  , worldTime
                  , worldWindowSize
                  , worldObjects
                  , worldPlanets
                  , worldAnomalies
                  , worldNewObjectCoords
                  , worldNewObjectPreviewCoords
                  , newWorld
                  , load
                  , add
                  , previewNewObj
                  , fire
                  , move
                  ) where

import Control.Lens
import Control.Monad.State (State, get)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Vect.Float as V

data Obj = Obj { _objPosition :: Vec2
               , _objVelocity :: Vec2
               }
makeLenses ''Obj

data Planet = Planet { _planetPosition :: Vec2
                     , _planetMass :: Float
                     , _planetRadius :: Float
                     }
makeLenses ''Planet

data Anomaly = Anomaly { _anomalyPosition :: Vec2
                       , _anomalyRadius :: Float
                       }
makeLenses ''Anomaly

data World = World { _worldTime :: Int
                   , _worldWindowSize :: (Int, Int)
                   , _worldObjects :: [Obj]
                   , _worldPlanets :: [Planet]
                   , _worldAnomalies :: [Anomaly]
                   , _worldNewObjectCoords :: Maybe Vec2
                   , _worldNewObjectPreviewCoords :: Maybe Vec2
                   }
makeLenses ''World

-- public

newWorld :: World
newWorld = World { _worldTime = 0
                 , _worldWindowSize = (800, 800)
                 , _worldObjects = []
                 , _worldPlanets = []
                 , _worldAnomalies = []
                 , _worldNewObjectCoords = Nothing
                 , _worldNewObjectPreviewCoords = Nothing
                 }

load :: [Planet] -> [Anomaly] -> State World ()
load planets anomalies = do
    worldPlanets .= planets
    worldAnomalies .= anomalies

add :: Integral a => a -> a -> State World ()
add x y = do
    w <- get
    let pos = Just $ fromMousePosition x y w
    worldNewObjectCoords .= pos
    worldNewObjectPreviewCoords .= pos

previewNewObj :: Integral a => a -> a -> State World ()
previewNewObj x y = do
    w <- get
    worldNewObjectPreviewCoords .= (Just $ fromMousePosition x y w)

fire :: Integral a => a -> a -> State World ()
fire x y = do
    w <- get
    let (Just coords) = w^.worldNewObjectCoords
        velocity = 0.005 *& (fromMousePosition x y w &- coords)
    worldObjects .= Obj coords velocity : w^.worldObjects
    worldNewObjectCoords .= Nothing
    worldNewObjectPreviewCoords .= Nothing

move :: Int -> State World ()
move time = do
    w <- get
    zoom (worldObjects.traversed) $ do
        o <- get
        let combinedGravityVector = vecSum $ map (getGravityVector o) $ w^.worldPlanets
        objVelocity %= (&+ combinedGravityVector)
        let dt = getElapsedTime o w time
        objPosition %= (&+ (dt *& (o^.objVelocity)))
    worldTime .= time

-- private

g = 6.67384e-11

getGravityVector :: Obj -> Planet -> Vec2
getGravityVector obj planet = force *& unitVector
    where force = g * (planet^.planetMass) / distance (obj^.objPosition) (planet^.planetPosition) ^ 2
          unitVector = normalize $ (planet^.planetPosition) &- (obj^.objPosition)

getElapsedTime :: Obj -> World -> Int -> Float
getElapsedTime obj world time = k * elapsedTime
    where k = maybe 1.0 (^ 2) $ find (< 1.0) distances
          distances = fmap (\a -> distance (obj^.objPosition) (a^.anomalyPosition) / (a^.anomalyRadius)) $ world^.worldAnomalies
          elapsedTime = fromIntegral $ time - world^.worldTime

fromMousePosition :: Integral a => a -> a -> World -> Vec2
fromMousePosition x y world = mkVec2 (normalizeDim x w, - (normalizeDim y h))
    where (w, h) = world^.worldWindowSize
          normalizeDim a b = (fromIntegral a / fromIntegral b) * 2 - 1
