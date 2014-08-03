{-# LANGUAGE TemplateHaskell #-}
module Gvty.World ( Obj()
                  , objPosition
                  , objVelocity
                  , Planet()
                  , planetPosition
                  , planetRadius
                  , Anomaly()
                  , anomalyPosition
                  , anomalyRadius
                  , World()
                  , worldWindowSize
                  , worldObjects
                  , worldPlanets
                  , worldAnomalies
                  , worldNewObjectCoords
                  , worldNewObjectPreviewCoords
                  , newWorld
                  , add
                  , previewNewObj
                  , fire
                  , move
                  ) where

import Control.Lens
import Control.Monad.State (State, get)
import Data.List (find)
import Data.Maybe (fromMaybe)

import Gvty.Utilities

data Obj = Obj { _objPosition :: (Float, Float)
               , _objVelocity :: Vector
               }
makeLenses ''Obj

data Planet = Planet { _planetPosition :: (Float, Float)
                     , _planetMass :: Float
                     , _planetRadius :: Float
                     }
makeLenses ''Planet

data Anomaly = Anomaly { _anomalyPosition :: (Float, Float)
                       , _anomalyRadius :: Float
                       }
makeLenses ''Anomaly

data World = World { _worldTime :: Int
                   , _worldWindowSize :: (Int, Int)
                   , _worldObjects :: [Obj]
                   , _worldPlanets :: [Planet]
                   , _worldAnomalies :: [Anomaly]
                   , _worldNewObjectCoords :: Maybe (Float, Float)
                   , _worldNewObjectPreviewCoords :: Maybe (Float, Float)
                   }
makeLenses ''World

-- public

newWorld :: World
newWorld = World { _worldTime = 0
                 , _worldWindowSize = (800, 800)
                 , _worldObjects = []
                 , _worldPlanets = [ Planet (-0.2, 0.2) 100000 0.2
                                   , Planet (0.2, -0.2) 100000 0.2
                                   ]
                 , _worldAnomalies = [Anomaly (-0.5, -0.5) 0.4]
                 , _worldNewObjectCoords = Nothing
                 , _worldNewObjectPreviewCoords = Nothing
                 }

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
        velocity = 0.005 *> fromPoints coords (fromMousePosition x y w)
    worldObjects .= Obj coords velocity : w^.worldObjects
    worldNewObjectCoords .= Nothing
    worldNewObjectPreviewCoords .= Nothing

move :: Int -> State World ()
move time = do
    w <- get
    zoom (worldObjects.traversed) $ do
        o <- get
        let gravityVector = foldr1 (<+>) $ map (getGravity o) $ w^.worldPlanets
        objVelocity %= (<+> gravityVector)
        let dt = getElapsedTime o w time
        objPosition %= offset (dt *> (o^.objVelocity))
    worldTime .= time

-- private

g = 6.67384e-11

getGravity :: Obj -> Planet -> Vector
getGravity obj planet = force *> unitVector
    where force = g * (planet^.planetMass) / distance (obj^.objPosition) (planet^.planetPosition) ^ 2
          unitVector = normalize $ fromPoints (obj^.objPosition) (planet^.planetPosition)

getElapsedTime :: Obj -> World -> Int -> Float
getElapsedTime obj world time = k * elapsedTime
    where k = maybe 1.0 (^ 2) $ find (< 1.0) distances
          distances = fmap (\a -> distance (obj^.objPosition) (a^.anomalyPosition) / (a^.anomalyRadius)) $ world^.worldAnomalies
          elapsedTime = fromIntegral $ time - world^.worldTime

fromMousePosition :: Integral a => a -> a -> World -> (Float, Float)
fromMousePosition x y world = (normalizeDim x w, - (normalizeDim y h))
    where (w, h) = world^.worldWindowSize
          normalizeDim a b = (fromIntegral a / fromIntegral b) * 2 - 1
