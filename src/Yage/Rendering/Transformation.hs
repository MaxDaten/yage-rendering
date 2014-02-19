{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.Transformation where

import Yage.Prelude
import Yage.Math


type Orientation  = Quaternion Float
type Scale        = V3 Float
type Position     = V3 Float

data Transformation = Transformation
    { _transPosition    :: !Position
    , _transOrientation :: !Orientation
    , _transScale       :: !Scale
    } deriving ( Show, Typeable )


makeLenses ''Transformation

idTransformation :: Transformation
idTransformation = Transformation zero (axisAngle (V3 0 1 0) (deg2rad 0)) (V3 1 1 1)
