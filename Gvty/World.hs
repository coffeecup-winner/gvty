{-# LANGUAGE TemplateHaskell #-}
module Gvty.World ( Obj()
                  , objPosition
                  , objVelocity
                  , Planet()
                  , planetPosition
                  , planetRadius
                  , World()
                  , worldWindowSize
                  , worldObjects
                  , worldPlanets
                  , worldNewObjectCoords
                  , newWorld
                  , add
                  , fire
                  , move
                  ) where

import Control.Lens
import Control.Monad.State (State, get)

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

data World = World { _worldTime :: Int
                   , _worldWindowSize :: (Int, Int)
                   , _worldObjects :: [Obj]
                   , _worldPlanets :: [Planet]
                   , _worldNewObjectCoords :: Maybe (Float, Float)
                   }
makeLenses ''World

-- public

newWorld :: World
newWorld = World { _worldTime = 0
                 , _worldWindowSize = (800, 800)
                 , _worldObjects = []
                 , _worldPlanets = [Planet (0, 0) 100000 0.2]
                 , _worldNewObjectCoords = Nothing
                 }

add :: Integral a => a -> a -> State World ()
add x y = do
    w <- get
    worldNewObjectCoords .= (Just $ fromMousePosition x y w)

fire :: Integral a => a -> a -> State World ()
fire x y = do
    w <- get
    let (Just coords) = w^.worldNewObjectCoords
        velocity = 0.01 *> fromPoints coords (fromMousePosition x y w)
    worldObjects .= Obj coords velocity : w^.worldObjects
    worldNewObjectCoords .= Nothing

move :: Int -> State World ()
move time = do
    w <- get
    let dt = fromIntegral $ time - w^.worldTime
    worldTime .= time
    zoom (worldObjects.traversed) $ do
        o <- get
        let gravityVector = foldr1 (<+>) $ map (getGravity o) $ w^.worldPlanets
        objVelocity %= (<+> gravityVector)
        objPosition %= offset (dt *> (o^.objVelocity))

-- private

g = 6.67384e-11

getGravity :: Obj -> Planet -> Vector
getGravity obj planet = force *> unitVector
    where force = g * (planet^.planetMass) / distance (obj^.objPosition) (planet^.planetPosition) ^ 2
          unitVector = normalize $ fromPoints (obj^.objPosition) (planet^.planetPosition)

fromMousePosition :: Integral a => a -> a -> World -> (Float, Float)
fromMousePosition x y world = (normalizeDim x w, - (normalizeDim y h))
    where (w, h) = world^.worldWindowSize
          normalizeDim a b = (fromIntegral a / fromIntegral b) * 2 - 1
