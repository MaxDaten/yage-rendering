module Yage.Rendering.Primitives.Basic where

import Yage.Prelude hiding (Index)

import Data.List (map, take, length, (!!), repeat, zipWith4, unzip)

import Yage.Rendering.Types
import Yage.Rendering.Lenses
import Yage.Rendering.VertexSpec
import Yage.Math


white :: (Fractional a) => V4 a
white = V4 1.0 1.0 1.0 1.0
-- f = front; h = hidden; t = top; b = bottom; r = right; l = left
f, h, t, b, r, l :: Fractional a => a
f     =  1.0
h     = -1.0
t     =  1.0
b     = -1.0
r     =  1.0
l     = -1.0
tlf, trf, brf, blf, tlh, trh, brh, blh :: (Fractional a) => V3 a
tlf   = V3 l t f
trf   = V3 r t f
brf   = V3 r b f
blf   = V3 l b f
      
tlh   = V3 l t h
trh   = V3 r t h
brh   = V3 r b h
blh   = V3 l b h



uv00, uv01, uv10, uv11 :: (Fractional a) => V2 a
uv00  = 0 
uv01  = V2 0 1
uv10  = V2 1 0
uv11  = 1



data Face v = Face v v v v
faceToVertexList :: Face v -> [v]
faceToVertexList (Face v0 v1 v2 v3) = [v0, v1, v2, v3]


---------------------------------------------------------------------------------------------------


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
pushToBack toMesh fromMesh =
  let vertexCount = length $ toMesh^.mDataVertices
  in toMesh & mDataVertices  <>~ fromMesh^.mDataVertices
            & mDataIndices   <>~ map (+vertexCount) (fromMesh^.mDataIndices)
            & mDataTriCount   +~ fromMesh^.mDataTriCount

extractMeshByIndices :: MeshData v -> MeshData v
extractMeshByIndices mesh = 
  let verts'  = map ((mesh^.mDataVertices)!!) $ mesh^.mDataIndices
      count   = length verts'
      ixs'    = [0..count]
  in  mDataVertices .~ verts'
    $ mDataIndices  .~ ixs'
    $ mDataTriCount .~ count `div` 3
    $ mesh