{-# LANGUAGE GADTs                    #-}
module Yage.Rendering.Lighting where

import Yage.Prelude

import Linear

data Light where
    Light :: { lightType           :: LightType
             , lightAttribs        :: LightAttributes
             } -> Light

data LightAttributes where
    LightAttributes ::
        { lightAmbientColor   :: V3 Double
        , lightDiffuseColor   :: V3 Double
        , lightSpecularColor  :: V3 Double
        , quadricAttenuation  :: Double   -- 1 linear, recommended 2 or 3
        } -> LightAttributes 
{--

alpha = distance / radius
damping_factor = 1.0 - pow(alpha,beta)
final_intensity = attenuation(distance) * damping_factor
beta = 1 :: linear

--}

data LightType where
    Pointlight  :: { pLightPosition  :: V3 Double 
                   , pLightRadius    :: Double
                   } -> LightType
    
    Spotlight   :: { sLightPosition  :: V3 Double
                   , sLightDirection :: V3 Double
                   , sLightCutoff    :: Double 
                   } -> LightType
    
    Directional :: { dLightDirection :: V3 Double
                   } -> LightType

