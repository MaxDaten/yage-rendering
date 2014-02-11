module Yage.Rendering.Primitives.Sphere where

import Yage.Prelude

import Yage.Rendering.Primitives.Basic
import Yage.Rendering.Primitives.Icosahedron
import Yage.Rendering.Mesh
import Yage.Rendering.Types
import Yage.Rendering.VertexSpec


geodesicSphereMesh :: NormalSmoothness -> Int -> Float -> MeshData Vertex3P3N
geodesicSphereMesh smoothness iter = 
    flip trianglesToMesh (normalCalculator smoothness) . (triangulate iter) . icosahedron