module Yage.Rendering.Primitives.Icosahedron where

import Yage.Prelude
import Data.List
import Yage.Math
import Yage.Rendering.Primitives.Basic
import Yage.Rendering.Types
import Yage.Rendering.VertexSpec



icosahedron :: (Floating v, Enum v, Show v) => Float -> [Triangle (V3 v)]
icosahedron radius = top ++ middleTop ++ middleBottom ++ bottom 
    where r             = realToFrac radius
          north         = V3 0   r  0
          south         = V3 0 (-r) 0
          topv          = [ V3 (r * cos a * sin theta) (  r  * cos theta) (r * sin a * sin theta)   | a <- init [ 0     , 2 * pi / 5 .. 2 * pi ] ]
          botv          = [ V3 (r * cos a * sin theta) ((-r) * cos theta) (r * sin a * sin theta)   | a <- init [ pi / 5, 3 * pi / 5 .. 2 * pi + pi / 5 ] ]
          V3 _ y _      = signorm $ V3 0 1 ((1 + sqrt 5)/2)
          theta         = acos y
          top           = [ Triangle north a b | (a, b) <- zip topv (cycle topv) ]
          bottom        = [ Triangle south a b | (a, b) <- zip (cycle botv) (botv) ]
          middleTop     = zipWith3 Triangle (cycle topv) topv (cycle botv)
          middleBottom  = zipWith3 Triangle botv (cycle botv) (topv)
          cycle list    = last list : init list


icosahedronMesh :: NormalSmoothness -> Float -> MeshData Vertex3P3N
icosahedronMesh smoothness = flip trianglesToMesh (normalCalculator smoothness) . icosahedron
