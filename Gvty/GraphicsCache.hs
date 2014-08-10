{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gvty.GraphicsCache ( GraphicsCache
                          , spheresCache
                          , newGraphicsCache
                          , memoized
                          ) where

import Control.Lens
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.Vect.Float
import Graphics.UI.GLUT

data GraphicsCache = GraphicsCache { _spheresCache :: M.Map (Int, Float, Float) [Vec3]
                                   }
makeLenses ''GraphicsCache

newGraphicsCache :: GraphicsCache
newGraphicsCache = GraphicsCache M.empty

memoized :: Ord a => IORef GraphicsCache -> Lens' GraphicsCache (M.Map a b) -> (a -> b) -> a -> IO b
memoized cacheRef lens f key = do
    cache <- get cacheRef
    let cached = M.lookup key $ cache^.lens
    maybe
        (do let result = f key
            cacheRef $~! (lens %~ M.insert key result)
            return result)
        return cached
