{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Rendering.Primitives
  (cubeMesh, quadMesh
  , makeMeshfromSpare, processSpareVerts
  ) where

import Yage.Prelude -- hiding (id)
import Control.Lens hiding (Index)

import Data.List ((++), reverse, map, take, length, (!!), concat, replicate, repeat, zipWith4, unzip)

import Linear (V2(..), V3(..), V4(..), R3(_xyz), point)

import Yage.Rendering.Types
import Yage.Rendering.VertexSpec
import Yage.Math

one  = 1.0
zero = 0.0
-- f = front; h = hidden; t = top; b = bottom; r = right; l = left
f     =  one
h     = -one
t     =  one
b     = -one
r     =  one
l     = -one

uv00  = V2 zero zero 
uv01  = V2 zero one
uv10  = V2 one zero 
uv11  = V2 one one 
white = (V4 1.0 1.0 1.0 1.0)

-------------------------------------------------------------------------------

cubeMesh :: Mesh Vertex4342
cubeMesh = 
    let 
        ltf   = (V3 l t f, uv00)
        rtf   = (V3 r t f, uv01)
        rbf   = (V3 r b f, uv11)
        lbf   = (V3 l b f, uv10)
        lth   = (V3 l t h, uv01)
        rth   = (V3 r t h, uv00)
        rbh   = (V3 r b h, uv10)
        lbh   = (V3 l b h, uv11)
        verts = over (mapped._1) point 
                  [ ltf, lbf, rbf, rtf
                  , lth, rth, rbh, lbh
                  ]
        frontFace = [0, 1, 2, 2, 3, 0] :: [Int]
        leftFace  = [0, 4, 7, 7, 1, 0] :: [Int]
        rightFace = [2, 6, 5, 5, 3, 2] :: [Int]
        topFace   = [0, 3, 5, 5, 4, 0] :: [Int]
        bottomFace= [1, 2, 6, 6, 7, 1] :: [Int]
        ixs       = frontFace
                  ++ leftFace
                  ++ topFace
                  ++ reverse (map (+4) frontFace) -- back
                  ++ rightFace
                  ++ bottomFace
    in makeMeshfromSpare "cube" (traceShow' verts) (traceShow' ixs) white



quadMesh :: Mesh Vertex4342
quadMesh = 
    let tl    = (V3 (-one)   one  zero, uv01)
        tr    = (V3   one    one  zero, uv11)
        br    = (V3   one  (-one) zero, uv10)
        bl    = (V3 (-one) (-one) zero, uv00)
        verts = over (mapped._1) point [tl, bl, br, tr]
        ixs   = [ 0, 1, 2
                , 2, 3, 0
                ]
    in traceShow' $ makeMeshfromSpare "quad" verts ixs white


-------------------------------------------------------------------------------


makeMeshfromSpare :: String -> [(Position4f, Texture2f)] -> [Index] -> Color4f -> Mesh Vertex4342
makeMeshfromSpare id verts ixs color =
    mkMesh id (processSpareVerts verts ixs color) $ take (length ixs) [0..]



-- | takes spare 3d-points (without duplicates) and the indices
-- to construct the adequate attributes to be processed by opengl 
processSpareVerts :: [(Position4f, Texture2f)] -> [Index] -> Color4f -> [Vertex4342]
processSpareVerts vs' ixs color = 
    let (vs, ts) = unzip $ extract vs' ixs
        ns = (normals $ vs^..traverse._xyz)^..traverse.replicated 3 
        cs = repeat color
    in zipWith4 Vertex vs ns cs ts
    where 
      extract :: [(Position4f, Texture2f)] -> [Index] -> [(Position4f, Texture2f)]
      extract vs = map (vs!!)

