{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.Transformation where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Control.Applicative

type Orientation a = Quaternion a
type Scale a       = V3 a
type Position a    = V3 a

data Transformation a = Transformation
    { _transPosition    :: !(Position a)
    , _transOrientation :: !(Orientation a)
    , _transScale       :: !(Scale a)
    } deriving ( Show, Eq, Ord, Typeable, Functor )


makeLenses ''Transformation

idTransformation :: (RealFloat a, Epsilon a) => Transformation a
idTransformation = Transformation zero 1 1


transformationMatrix :: Num a => Getter (Transformation a) (M44 a)
transformationMatrix = to get where
    get trans = 
        let scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        in transM !*! scaleM

inverseTransformation :: (Conjugate a, RealFloat a) => Transformation a -> Transformation a
inverseTransformation t =
    t   & transScale            %~ recip
        & transPosition         %~ negate
        & transOrientation._ijk %~ negate


instance Applicative Transformation where
    pure a = Transformation (pure a) (pure a) (pure a)
    (Transformation fp fo fs) <*> (Transformation p o s) = Transformation (fp <*> p) (fo <*> o) (fs <*> s)

instance RealFloat a => Num (Transformation a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger 

instance (RealFloat a, Epsilon a) => Epsilon (Transformation a) where
    nearZero (Transformation p o s) = nearZero p && nearZero o && nearZero s  
