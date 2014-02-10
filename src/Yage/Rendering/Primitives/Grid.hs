module Yage.Rendering.Primitives.Grid where

import Yage.Prelude hiding (Index)

import Data.Foldable

import Linear (V2(..), V3(..))

import Yage.Rendering.Types
import Yage.Rendering.VertexSpec
import Yage.Math

import Yage.Rendering.Primitives.Basic

-- | creates a grid along the xz plane with its center in origin and 1/1 dimension
-- divisions along x and y. for a propper positioning of the center in origin, divisions
-- sould be even
-- TODO offset left
gridMesh :: V2 Int -> MeshData Vertex3P3N3C4T2
gridMesh divs@(V2 xdiv zdiv) 
  | xdiv < 1 || zdiv < 1 = error "invalid divisions"
  | otherwise = 
    let V2 xStep zStep   = 1.0 / (fromIntegral <$> divs)
        ixs              = genIdxs
        count            = xdiv*zdiv*2
        verts            = genVerts xStep zStep (-0.5, -0.5)
    in MeshData verts ixs count
  where 
    genVerts xStep zStep (left, back) =
          [Vertex v n c t | z <- [0.0 .. fromIntegral zdiv], x <- [0.0 .. fromIntegral xdiv]
              , let v = V3 (left + x * xStep) 0.0 (back + z * zStep) 
              , let n = V3 0 1.0 0
              , let c = white
              , let t = V2 (x * xStep) (z * zStep) 
              ]
    genIdxs = 
      concat [[ leftBack, rightBack, rightFront, rightFront, leftFront, leftBack]
              | row <- [0..zdiv-1], col <- [0..xdiv-1]
              , let stride      = xdiv + 1
              , let leftBack    = row * stride + col
              , let rightBack   = row * stride + col + 1
              , let leftFront   = (row + 1) * stride + col
              , let rightFront  = (row + 1) * stride + col + 1
              ]
