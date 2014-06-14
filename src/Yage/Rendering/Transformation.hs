{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.Transformation where

import Yage.Prelude
import Yage.Lens
import Yage.Math hiding (lerp)
import qualified Linear (lerp, slerp) 
import Control.Applicative

data Transformation a = Transformation
    { _transPosition    :: !(V3 a)
    , _transOrientation :: !(Quaternion a)
    , _transScale       :: !(V3 a)
    } deriving ( Show, Eq, Ord, Typeable, Functor )


makeLenses ''Transformation

idTransformation :: RealFloat a => Transformation a
idTransformation = Transformation 0 1 1


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

instance RealFloat a => Default (Transformation a) where
    def = idTransformation


class LinearInterpolatable a where
    lerp :: Double -> a -> a -> a


instance RealFloat a => LinearInterpolatable (Transformation a) where
    lerp alpha u v = u & transPosition    .~ Linear.lerp (realToFrac alpha) (u^.transPosition) (v^.transPosition)
                       & transScale       .~ Linear.lerp (realToFrac alpha) (u^.transScale) (v^.transScale)
                       & transOrientation .~ Linear.slerp (u^.transOrientation) (v^.transOrientation) (realToFrac alpha)

