module Yage.Rendering.Primitives.Cone where

import Yage.Prelude

import Data.List
import Yage.Math

import Yage.Rendering.Types
import Yage.Rendering.VertexSpec
import Yage.Rendering.Primitives.Basic


cone :: (Floating v, Enum v) => Float -> Float -> Int -> [Triangle (V3 v)]
cone radius height divs =
    let h           = realToFrac height
        r           = realToFrac radius
        d           = realToFrac divs
        tip         = V3 0 h 0
        baseCenter  = V3 0 0 0
        basev       = [ V3 (r * cos a ) 0 (r * sin a) | a <- init [0, 2 * pi / d .. 2 * pi ] ]
        mantle      = [ Triangle tip a b        | (a, b) <- zip basev (shift basev) ]
        base        = [ Triangle baseCenter a b | (a, b) <- zip (shift basev) basev ] 
    in mantle ++ base

coneMesh :: Float -> Float -> Int -> MeshData Vertex3P3N
coneMesh radius height divs = trianglesToMesh (cone radius height divs) (normalCalculator FacetteNormals)