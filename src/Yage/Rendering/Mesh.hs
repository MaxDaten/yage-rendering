{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE FlexibleContexts            #-}

module Yage.Rendering.Mesh
  ( module Yage.Rendering.Mesh
  , module E
  ) where

import           Yage.Prelude                        hiding (Index)
import           Yage.Lens

import           GHC.Generics                        (Generic)
import           Foreign                             (Storable)
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector                         as V
import qualified Data.Digest.XXHash                  as XH
import qualified Data.Vector.Storable.ByteString     as BS
import qualified Data.ByteString                     as BS


import Yage.Geometry.Vertex
import Yage.Geometry
import Yage.Geometry.Elements as E (Triangle(..))


--type GetVertexAttributes v = (MeshData v -> [VertexAttribute])

---------------------------------------------------------------------------------------------------
type TriMesh v = Mesh v (Triangle Int) 
type MeshHash = XH.XXHash
type MeshId   = String

data Mesh vertex prim = Mesh
    { _meshId         :: !MeshId
    , _meshVertices   :: !(VS.Vector (Vertex vertex))
    , _meshPrimitives :: !(VS.Vector prim)            -- | primitives like Triangle with indices to the vertices
    , _meshHash       :: !MeshHash
    }
    deriving ( Typeable, Generic )

makeLenses ''Mesh

instance Show (Mesh v p) where
    show Mesh{..} =
        format "Mesh {id = {0}, attribs = N/A, meshHash = {1}}"
               [show _meshId, show _meshHash]

instance Eq (Mesh v p) where
    a == b = _meshId a == _meshId b

instance Ord (Mesh v p) where
    compare a b = compare (_meshId a) (_meshId b)


vertexCount :: (Storable (Vertex v)) => Mesh v p -> Int
vertexCount Mesh{_meshVertices} = VS.length _meshVertices

elementCount :: (Storable p) => Mesh v p -> Int
elementCount Mesh{_meshPrimitives} = VS.length _meshPrimitives

indexCount :: (Storable (p a), Foldable p) => Mesh v (p a) -> Int
indexCount Mesh{_meshPrimitives} = VS.sum $ VS.map (lengthOf folded) _meshPrimitives


---------------------------------------------------------------------------------------------------

makeMesh :: (Storable (Vertex v), Storable p) => String -> [Vertex v] -> [p] -> Mesh v p
makeMesh ident verts elems = makeMeshV ident (V.fromList verts) (V.fromList elems)

makeSimpleTriMesh :: (Storable (Vertex v)) => String -> [Vertex v] -> TriMesh v
makeSimpleTriMesh ident = makeMeshGeo ident . makeSimpleTriGeo . V.fromList

makeMesh' :: (Storable (Vertex v), Storable p) => [Vertex v] -> [p] -> Mesh v p
makeMesh' verts elems = makeMeshV' (V.fromList verts) (V.fromList elems)

makeMeshGeo :: (Storable (Vertex v), Storable p) => String -> Geometry (Vertex v) p -> Mesh v p
makeMeshGeo ident Geometry{..} = makeMeshV ident geoVertices geoElements


makeMeshV :: (Storable (Vertex v), Storable p) => String -> V.Vector (Vertex v) -> V.Vector p -> Mesh v p
makeMeshV ident verts elems = 
    let vVec        = V.convert verts
        eVec        = V.convert elems
        hash        = XH.xxHash' $ (BS.vectorToByteString vVec) `BS.append` (BS.vectorToByteString eVec)
    in Mesh ident vVec eVec hash


makeMeshV' :: (Storable (Vertex v), Storable p) => V.Vector (Vertex v) -> V.Vector p -> Mesh v p
makeMeshV' verts elems = 
    let vVec        = V.convert verts
        eVec        = V.convert elems
        hash        = XH.xxHash' $ (BS.vectorToByteString vVec) `BS.append` (BS.vectorToByteString eVec)
    in Mesh (show hash) vVec eVec hash


emptyMesh :: (Storable (Vertex v), Storable p) => Mesh v p
emptyMesh = Mesh "" VS.empty VS.empty 0


updateMesh :: (Storable (Vertex v), Storable p) => Mesh v p -> [Vertex v] -> [p] -> Mesh v p
updateMesh mesh verts elements =  
    let vVec        = VS.fromList verts
        eVec        = VS.fromList elements
        hash        = XH.xxHash' $ (BS.vectorToByteString vVec) `BS.append` (BS.vectorToByteString eVec)
    in mesh{_meshVertices = vVec, _meshPrimitives = eVec, _meshHash = hash}

pushToBack :: (Storable (Vertex v), Storable p) => Mesh v p -> [Vertex v] -> [p] -> Mesh v p
pushToBack mesh@Mesh{ _meshVertices, _meshPrimitives, _meshHash } verts elements =
    let newVerts = _meshVertices VS.++ VS.fromList verts
        newElems = _meshPrimitives VS.++ VS.fromList elements
        newHash = XH.xxHash' $ (BS.vectorToByteString $ newVerts) `BS.append` (BS.vectorToByteString newElems)
    in mesh{ _meshVertices = newVerts, _meshPrimitives = newElems, _meshHash = newHash }

getMeshElementIndices :: (Storable (f Int), Foldable f) => Mesh v (f Int) -> VS.Vector Int
getMeshElementIndices mesh = VS.concatMap (VS.fromList . (toListOf folded)) $ mesh^.meshPrimitives
{--

extractMeshByIndices :: MeshData v -> MeshData v
extractMeshByIndices mesh = 
  let verts'  = map ((mesh^.mDataVertices)!!) $ mesh^.mDataIndices
      count   = length verts'
      ixs'    = [0..count]
  in mesh & mDataVertices .~ verts'
          & mDataIndices  .~ ixs'
          & mDataTriCount .~ count `div` 3
--}
