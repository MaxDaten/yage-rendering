{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Yage.Rendering.Primitives.Cube where

import Yage.Prelude hiding (Index)

import Data.List (foldr)
import Linear (V3(..))
import Yage.Rendering.Types
import Yage.Rendering.Primitives.Basic
import Yage.Rendering.VertexSpec
import Yage.Rendering.Mesh

---------------------------------------------------------------------------------------------------
-- Primitives


cubeMesh :: (Real a) => V3 a -> MeshData Vertex3P3N3C4T2
cubeMesh dim = 
  let V3 x y z = (realToFrac <$> dim) / 2.0
  in foldr addFaceToMesh emptyMeshData 
    [ Face (Vertex (V3 (-x)   y   z ) () white uv01) (Vertex (V3 (-x) (-y)   z ) () white uv00) (Vertex (V3   x  (-y)   z ) () white uv10) (Vertex (V3   x   y   z ) () white uv11)
    , Face (Vertex (V3 (-x)   y (-z)) () white uv01) (Vertex (V3 (-x) (-y) (-z)) () white uv00) (Vertex (V3 (-x) (-y)   z ) () white uv10) (Vertex (V3 (-x)  y   z ) () white uv11)
    , Face (Vertex (V3   x    y   z ) () white uv01) (Vertex (V3   x  (-y)   z ) () white uv00) (Vertex (V3   x  (-y) (-z)) () white uv10) (Vertex (V3   x   y (-z)) () white uv11)
    , Face (Vertex (V3 (-x)   y (-z)) () white uv01) (Vertex (V3 (-x)   y    z ) () white uv00) (Vertex (V3   x    y    z ) () white uv10) (Vertex (V3   x   y (-z)) () white uv11)
    , Face (Vertex (V3 (-x) (-y)  z ) () white uv01) (Vertex (V3 (-x) (-y) (-z)) () white uv00) (Vertex (V3   x  (-y) (-z)) () white uv10) (Vertex (V3   x (-y)  z ) () white uv11)
    , Face (Vertex (V3   x    y (-z)) () white uv01) (Vertex (V3   x  (-y) (-z)) () white uv00) (Vertex (V3 (-x) (-y) (-z)) () white uv10) (Vertex (V3 (-x)  y (-z)) () white uv11)
    ]


