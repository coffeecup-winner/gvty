{-# LANGUAGE OverloadedStrings #-}
module Gvty.Serialization where

import Control.Applicative
import Control.Monad.State
import Data.Aeson

import Gvty.Utilities
import Gvty.World

instance FromJSON Vector where
    parseJSON (Object x) = Vector <$> x .: "coords"

instance FromJSON Obj where
    parseJSON (Object x) = Obj <$> x .: "position"
                               <*> x .: "velocity"

instance FromJSON Planet where
    parseJSON (Object x) = Planet <$> x .: "position"
                                  <*> x .: "mass"
                                  <*> x .: "radius"

instance FromJSON Anomaly where
    parseJSON (Object x) = Anomaly <$> x .: "position"
                                   <*> x .: "radius"

instance FromJSON World where
    parseJSON (Object x) = World <$> x .:? "time" .!= 0
                                 <*> x .:? "size" .!= (800, 800)
                                 <*> x .:? "objects" .!= []
                                 <*> x .: "planets"
                                 <*> x .: "anomalies"
                                 <*> x .:? "new_obj_coords" .!= Nothing
                                 <*> x .:? "new_obj_preview_coords" .!= Nothing
