module Gvty.Utilities ( tmap
                      ) where

tmap :: (a -> b -> a) -> (b, b) -> (a, a) -> (a, a)
tmap f (a1, a2) (x1, x2) = (f x1 a1, f x2 a2)
