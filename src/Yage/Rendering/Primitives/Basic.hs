module Yage.Rendering.Primitives.Basic where

import Yage.Prelude hiding (Index)

import Data.List

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


data Triangle v = Triangle v v v
triangleToList :: Triangle v -> [v]
triangleToList (Triangle v0 v1 v2) = [v0, v1, v2]

data Face v = Face v v v v
faceToList :: Face v -> [v]
faceToList (Face v0 v1 v2 v3) = [v0, v1, v2, v3]

data NormalSmoothness = FacetteNormals | SphericalNormals

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
  in meshData &  mDataVertices  <>~ map (\(Vertex p () c t) -> Vertex p normal c t) (faceToList face) -- ++ meshData^.vertices 
              & mDataIndices   <>~ map (vertexCount+) [0, 1, 2, 2, 3, 0] -- ++ map (+4) indices
              & mDataTriCount  +~ 2

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
  in mesh & mDataVertices .~ verts'
          & mDataIndices  .~ ixs'
          & mDataTriCount .~ count `div` 3


trianglesToMesh :: (Real v) => [Triangle (V3 v)] -> (Triangle (V3 v) -> (Normal3f, Normal3f, Normal3f)) -> MeshData Vertex3P3N
trianglesToMesh tris triNormals = MeshData
  { _mDataVertices  = concatMap toVertices tris
  , _mDataIndices   = take (3 * length tris) [0..]
  , _mDataTriCount  = length tris
  }
  where
  toVertices tri@(Triangle a b c) =
    let (na, nb, nc) = triNormals tri
    in [Vertex (realToFrac <$> a) na () (), Vertex (realToFrac <$> b) nb () (), Vertex (realToFrac <$> c) nc () ()] 


cut :: (Functor f, Num (f a), Num a, Epsilon a, Metric f, Floating a) 
    => a -> Triangle (f a) -> [Triangle (f a)]
cut r tri@(Triangle a b c) = [Triangle a ab ac, Triangle b bc ab, Triangle c ac bc, Triangle ab bc ac]
    where ab = hr *^ normalize (a + b)
          bc = hr *^ normalize (b + c)
          ac = hr *^ normalize (a + c)
          hr = r

 
triangulate :: (Functor f, Num (f a), Num a, Epsilon a, Metric f, Floating a) 
            => Int -> [Triangle (f a)] -> [Triangle (f a)]
triangulate iter src    = iterate subdivide src !! iter
    where subdivide     = concatMap cutR
          cutR          = cut (norm . fst3 . head $ src)
          fst3 (Triangle a _ _) = a


normalCalculator :: (Epsilon v, Floating v) => NormalSmoothness -> (Triangle (V3 v) -> (V3 v, V3 v, V3 v))
normalCalculator SphericalNormals = \(Triangle a b c) -> (normalize a, normalize b, normalize c) 
normalCalculator FacetteNormals   = \(Triangle a b c) -> let (n, _, _) = plainNormalForm c b a in (n, n, n)  
