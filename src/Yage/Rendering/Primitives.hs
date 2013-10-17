{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Rendering.Primitives where

import Yage.Prelude -- hiding (id)
import Control.Lens hiding (Index)

import Data.List ((++), reverse, map, take, length, (!!), concat, replicate, repeat, zipWith3)

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

white = (V4 1.0 1.0 1.0 1.0)

-------------------------------------------------------------------------------

cubeMesh :: Mesh Vertex434
cubeMesh = 
    let 
        ltf   = V3 l t f
        rtf   = V3 r t f
        rbf   = V3 r b f
        lbf   = V3 l b f
        lth   = V3 l t h
        rth   = V3 r t h
        rbh   = V3 r b h
        lbh   = V3 l b h
        verts = point 
                <$> [ ltf, lbf, rbf, rtf
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



quadMesh :: Mesh Vertex434
quadMesh = 
    let tl    = V3 (-one)   one  zero
        bl    = V3 (-one) (-one) zero
        br    = V3   one  (-one) zero
        tr    = V3   one    one  zero
        verts = point <$> [tl, bl, br, tr]
        ixs   = [0, 1, 2, 2, 3, 0]
    in makeMeshfromSpare "quad" verts ixs white


-------------------------------------------------------------------------------


makeMeshfromSpare :: String -> [Position4f] -> [Index] -> Color4f -> Mesh Vertex434
makeMeshfromSpare id verts ixs color =
    mkTriMesh id (processSpareVerts verts ixs color) $ take (length ixs) [0..]



-- | takes spare 3d-points (without duplicates) and the indices
-- to construct the adequate attributes to be processed by opengl 
processSpareVerts :: [Position4f] -> [Index] -> Color4f -> [Vertex434]
processSpareVerts vs' ixs color = 
    let vs = extract vs' ixs
        ns = (normals $ vs^..traverse._xyz)^..traverse.replicated 3 
        cs = repeat color
    in zipWith3 Vertex vs ns cs
    where 
      extract :: [Position4f] -> [Index] -> [Position4f]
      extract vs = map (vs!!)

