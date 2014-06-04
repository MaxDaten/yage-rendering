{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.Transformation where

import Yage.Prelude
import Yage.Lens
import Yage.Math


type Orientation a = Quaternion a
type Scale a       = V3 a
type Position a    = V3 a

data Transformation a = Transformation
    { _transPosition    :: !(Position a)
    , _transOrientation :: !(Orientation a)
    , _transScale       :: !(Scale a)
    } deriving ( Show, Typeable, Functor )


makeLenses ''Transformation

idTransformation :: (RealFloat a, Epsilon a) => Transformation a
idTransformation = Transformation zero (axisAngle (V3 0 1 0) (deg2rad 0)) (V3 1 1 1)


calcModelMatrix :: (Num a) => Transformation a -> M44 a
calcModelMatrix trans =
    let scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
    in transM !*! scaleM
