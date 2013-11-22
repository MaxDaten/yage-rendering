{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Yage.Rendering.Primitives
  ( cubeMesh, quadMesh
  , makeMeshfromSpare, processSpareVerts, pushToBack, extractMeshByIndices
  ) where

import Yage.Prelude hiding (Index)

import Data.List (map, take, length, (!!), repeat, zipWith4, unzip)
import Data.Foldable

import Linear (V3(..), V4(..), R3(_xyz), point)

import Yage.Rendering.Types
import Yage.Rendering.Lenses
import Yage.Rendering.Mesh
import Yage.Rendering.VertexSpec
import Yage.Math

white = V4 1.0 1.0 1.0 1.0
-- f = front; h = hidden; t = top; b = bottom; r = right; l = left
f     =  1.0
h     = -1.0
t     =  1.0
b     = -1.0
r     =  1.0
l     = -1.0
tlf   = V3 l t f
trf   = V3 r t f
brf   = V3 r b f
blf   = V3 l b f
      
tlh   = V3 l t h
trh   = V3 r t h
brh   = V3 r b h
blh   = V3 l b h


-------------------------------------------------------------------------------

data Face v = Face v v v v
faceToVertexList :: Face v -> [v]
faceToVertexList (Face v0 v1 v2 v3) = [v0, v1, v2, v3]

data SimpleCubeDef f = SimpleCubeDef
  { front  :: f
  , left   :: f
  , right  :: f
  , top    :: f
  , bottom :: f
  , hidden :: f
  } deriving (Typeable, Functor, Foldable)

defaultCubeDef :: SimpleCubeDef (Face Vertex3P4C2T)
defaultCubeDef = SimpleCubeDef
    { front   = Face (Vertex tlf () white uv01) (Vertex blf () white uv00) (Vertex brf () white uv10) (Vertex trf () white uv11)
    , left    = Face (Vertex tlh () white uv01) (Vertex blh () white uv00) (Vertex blf () white uv10) (Vertex tlf () white uv11)
    , right   = Face (Vertex trf () white uv01) (Vertex brf () white uv00) (Vertex brh () white uv10) (Vertex trh () white uv11)
    , top     = Face (Vertex tlh () white uv01) (Vertex tlf () white uv00) (Vertex trf () white uv10) (Vertex trh () white uv11)
    , bottom  = Face (Vertex blf () white uv01) (Vertex blh () white uv00) (Vertex brh () white uv10) (Vertex brf () white uv11)
    , hidden  = Face (Vertex trh () white uv01) (Vertex brh () white uv00) (Vertex blh () white uv10) (Vertex tlh () white uv11)
    }

cubeMesh' :: SimpleCubeDef (Face Vertex3P4C2T) -> MeshData Vertex3P3N3C4T2
cubeMesh' def@SimpleCubeDef{..} = foldr addFaceToMesh emptyMeshData def

cubeMesh :: MeshData Vertex3P3N3C4T2
cubeMesh = cubeMesh' defaultCubeDef



quadMesh :: MeshData Vertex3P3N3C4T2
quadMesh = 
    let tl    = (V3 (-1.0)   1.0  0.0, uv10)
        tr    = (V3   1.0    1.0  0.0, uv11)
        br    = (V3   1.0  (-1.0) 0.0, uv01)
        bl    = (V3 (-1.0) (-1.0) 0.0, uv00)
        verts = [tl, bl, br, tr]
        ixs   = [ 0, 1, 2
                , 2, 3, 0
                ]
    in makeMeshfromSpare verts ixs white


-------------------------------------------------------------------------------


makeMeshfromSpare :: [(Position3f, Texture2f)] -> [Index] -> Color4f -> MeshData Vertex3P3N3C4T2
makeMeshfromSpare verts ixs color =
    let vertCount = length ixs
        linIdxs   = take vertCount [0..]
        triCount  = vertCount `quot` 3
    in MeshData (processSpareVerts verts ixs color) linIdxs triCount



-- | takes spare 3d-points (without duplicates) and the indices
-- to construct the adequate attributes to be processed by opengl 
processSpareVerts :: [(Position3f, Texture2f)] -> [Index] -> Color4f -> [Vertex3P3N3C4T2]
processSpareVerts vs' ixs color = 
    let (vs, ts) = unzip $ extract vs' ixs
        ns = normals (vs^..traverse._xyz)^..traverse.replicated 3 
        cs = repeat color
    in zipWith4 Vertex vs ns cs ts
    where 
      extract :: [(Position3f, Texture2f)] -> [Index] -> [(Position3f, Texture2f)]
      extract vs = map (vs!!)


addFaceToMesh :: Face Vertex3P4C2T -> MeshData Vertex3P3N3C4T2 -> MeshData Vertex3P3N3C4T2
addFaceToMesh face@(Face v0 v1 v2 _) meshData = 
  let (normal, _, _)  = plainNormalForm (v2^.vPosition) (v1^.vPosition) (v0^.vPosition)
      vertexCount     = length $ meshData^.mDataVertices
  in   mDataVertices  <>~ map (\(Vertex p () c t) -> Vertex p normal c t) (faceToVertexList face) -- ++ meshData^.vertices 
     $ mDataIndices   <>~ map (vertexCount+) [0, 1, 2, 2, 3, 0] -- ++ map (+4) indices
     $ mDataTriCount  +~ 2
     $ meshData

addFaceToMesh _ _     = error "invalid face" 


pushToBack :: MeshData v -> MeshData v -> MeshData v
pushToBack to from =
  let vertexCount = length $ to^.mDataVertices
  in   mDataVertices  <>~ from^.mDataVertices
     $ mDataIndices   <>~ map (+vertexCount) (from^.mDataIndices)
     $ mDataTriCount   +~ from^.mDataTriCount
     $ to

extractMeshByIndices :: MeshData v -> MeshData v
extractMeshByIndices mesh = 
  let verts'  = map ((mesh^.mDataVertices)!!) $ mesh^.mDataIndices
      count   = length verts'
      ixs'    = [0..count]
  in  mDataVertices .~ verts'
    $ mDataIndices  .~ ixs'
    $ mDataTriCount .~ count `div` 3
    $ mesh
