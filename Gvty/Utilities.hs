--{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Gvty.Utilities ( Vector(..)
                      , fromPoints
                      , offset
                      , (<+>)
                      , (*>)
                      , distance
                      , norm
                      , normalize
                      ) where

import Data.Monoid

newtype Vector = Vector (Float, Float)

fromPoints :: (Float, Float) -> (Float, Float) -> Vector
fromPoints (x1, y1) (x2, y2) = Vector (x2 - x1, y2 - y1)

offset :: Vector -> (Float, Float) -> (Float, Float)
offset (Vector (vx, vy)) (x, y) = (x + vx, y + vy)

(<+>) :: Vector -> Vector -> Vector
(<+>) (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)

(*>) :: Float -> Vector -> Vector
(*>) k (Vector (x, y)) = Vector (k * x, k * y)

distance :: (Float, Float) -> (Float, Float) -> Float
distance p1 p2 = norm $ fromPoints p1 p2

norm :: Vector -> Float
norm (Vector (x, y)) = sqrt $ x^2 + y^2

normalize :: Vector -> Vector
normalize v@(Vector (x, y)) = let n = norm v in Vector (x / n, y / n)

instance Monoid Vector where
    mempty = Vector (0, 0)
    mappend = (<+>)
