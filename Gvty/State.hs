module Gvty.State ( State()
                  , newState
                  , add
                  , fire
                  , stateWindowSize
                  , setWindowSize
                  , statePoints
                  , move
                  , Obj()
                  , objPosition
                  ) where

import Graphics.UI.GLUT

data Obj = Obj { objPosition :: (GLfloat, GLfloat)
               , objVelocity :: (GLfloat, GLfloat)
               }

data State = State { stateTime :: Int
                   , stateWindowSize :: (GLint, GLint)
                   , statePoints :: [Obj]
                   }

-- public

newState :: State
newState = State 0 (800, 800) []

setWindowSize :: GLint -> GLint -> State -> State
setWindowSize w h (State time _ os) = State time (w, h) os

add :: GLint -> GLint -> State -> State
add x y state@(State time size@(w, h) os) = State time size ((Obj (toFloat x y state) (0, 0)) : os)

fire :: GLint -> GLint -> State -> State
fire x y state@(State time size ((Obj pos@(ox, oy) _):os)) = State time size ((Obj pos velocity) : os)
    where (nx, ny) = toFloat x y state
          velocity = ((nx - ox) / 10, (ny - oy) / 10)

move :: Int -> State -> State
move time (State prevTime size os) = State time size (map moveObj os)
    where moveObj (Obj (x, y) velocity@(vx, vy)) = Obj (x + vx, y + vy) velocity

-- private

toFloat :: GLint -> GLint -> State -> (GLfloat, GLfloat)
toFloat x y (State _ (w, h) _) = (normalizeDim x w, - (normalizeDim y h))
    where normalizeDim a b = (fromIntegral a / fromIntegral b) * 2 - 1
