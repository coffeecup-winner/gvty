{-# LANGUAGE TemplateHaskell #-}
module Gvty.World ( World()
                  , worldTime
                  , worldWindowSize
                  , worldObjects
                  , worldNewObjectCoords
                  , newWorld
                  , add
                  , fire
                  , move
                  , Obj()
                  , objPosition
                  , objVelocity
                  ) where

import Control.Lens
import Control.Monad.State (State, get)
import Graphics.UI.GLUT hiding (get)

import Gvty.Utilities

data Obj = Obj { _objPosition :: (GLfloat, GLfloat)
               , _objVelocity :: (GLfloat, GLfloat)
               }
makeLenses ''Obj

data World = World { _worldTime :: Int
                   , _worldWindowSize :: (GLint, GLint)
                   , _worldObjects :: [Obj]
                   , _worldNewObjectCoords :: Maybe (GLfloat, GLfloat)
                   }
makeLenses ''World

-- public

newWorld :: World
newWorld = World { _worldTime = 0
                 , _worldWindowSize = (800, 800)
                 , _worldObjects = []
                 , _worldNewObjectCoords = Nothing
                 }

add :: GLint -> GLint -> State World ()
add x y = do
    w <- get
    worldNewObjectCoords .= (Just $ toFloat x y w)

fire :: GLint -> GLint -> State World ()
fire x y = do
    w <- get
    let (Just coords) = w^.worldNewObjectCoords
        velocity = tmap (\dest src -> (dest - src) / 100) coords $ toFloat x y w
    worldObjects .= (Obj coords velocity) : w^.worldObjects
    worldNewObjectCoords .= Nothing

move :: Int -> State World ()
move time = do
    w <- get
    let dt = fromIntegral $ time - w^.worldTime
        moveObj obj = let (dx, dy) = obj^.objVelocity
                      in  objPosition %~ (\(x, y) -> (x + dx * dt, y + dy * dt)) $ obj
    worldTime .= time
    worldObjects.mapped %= moveObj

-- private

toFloat :: GLint -> GLint -> World -> (GLfloat, GLfloat)
toFloat x y world = (normalizeDim x w, - (normalizeDim y h))
    where (w, h) = world^.worldWindowSize
          normalizeDim a b = (fromIntegral a / fromIntegral b) * 2 - 1
