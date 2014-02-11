module Yage.Rendering.Primitives.Pyramid where

import Yage.Prelude

import Data.List
import Yage.Math

import Yage.Rendering.Types
import Yage.Rendering.VertexSpec
import Yage.Rendering.Primitives.Basic


pyramid :: (Floating v, Enum v, Real a) => V3 a -> [Triangle (V3 v)]
pyramid dim = 
    let (V3 x h z)  = (realToFrac <$> dim) / V3 2 1 2
        tip         = V3 0 h 0
        basev       = [ V3 (-x) 0 z, V3 x 0 z, V3 x 0 (-z), V3 (-x) 0 (-z) ]
        mantle      = [ Triangle tip a b  | (a, b)    <- zip (shift basev) basev ]
        base        = [ Triangle a b c    | (a, b, c) <- zip3 basev (shift basev) (shift $ shift basev) ] 
    in mantle ++ base


pyramidMesh :: (Real v) => V3 v -> MeshData Vertex3P3N
pyramidMesh dim = trianglesToMesh (pyramid dim) (normalCalculator FacetteNormals)