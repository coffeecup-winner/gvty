{-# LANGUAGE TemplateHaskell #-}
module Gvty.Utilities ( tmap
                      , Vector
                      , fromPoints
                      , offset
                      , (<+>)
                      , (*>)
                      , distance
                      , norm
                      , normalize
                      ) where

tmap :: (a -> b -> a) -> (b, b) -> (a, a) -> (a, a)
tmap f (a1, a2) (x1, x2) = (f x1 a1, f x2 a2)

type Vector = (Float, Float)

fromPoints :: (Float, Float) -> (Float, Float) -> Vector
fromPoints (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

offset :: Vector -> (Float, Float) -> (Float, Float)
offset (x2, y2) (x1, y1) = (x1 + x2, y1 + y2)

(<+>) :: Vector -> Vector -> Vector
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(*>) :: Float -> Vector -> Vector
(*>) k (x, y) = (k * x, k * y)

distance :: (Float, Float) -> (Float, Float) -> Float
distance p1 p2 = norm $ fromPoints p1 p2

norm :: Vector -> Float
norm (x, y) = sqrt $ x^2 + y^2

normalize :: Vector -> Vector
normalize v@(x, y) = let n = norm v in (x / n, y / n)
