{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE FlexibleContexts            #-}
module Yage.Rendering.Mesh where

import           Yage.Prelude                        hiding (Index)

import           GHC.Generics                        (Generic)
import           Foreign                             (Storable)
import qualified Data.Vector.Storable                as VS
import qualified Data.Vector                         as V
import qualified Data.Digest.XXHash                  as XH
import qualified Data.Vector.Storable.ByteString     as BS


import Yage.Geometry.Vertex


--type GetVertexAttributes v = (MeshData v -> [VertexAttribute])

---------------------------------------------------------------------------------------------------

type MeshHash = XH.XXHash
type MeshId   = String

data Mesh v = Mesh
    { _meshId         :: !MeshId
    , _meshData       :: !(VS.Vector (Vertex v))
    , _meshHash       :: !MeshHash
    }
    deriving ( Typeable, Generic )

instance Show (Mesh v) where
    show Mesh{..} =
        format "Mesh {id = {0}, attribs = N/A, meshHash = {1}}"
               [show _meshId, show _meshHash]

instance Eq (Mesh v) where
    a == b = _meshId a == _meshId b

instance Ord (Mesh v) where
    compare a b = compare (_meshId a) (_meshId b)


dataCount :: (Storable (Vertex v)) => Mesh v -> Int
dataCount Mesh{_meshData} = VS.length _meshData

---------------------------------------------------------------------------------------------------

makeMesh :: (Storable (Vertex v)) => String -> [Vertex v] -> Mesh v
makeMesh ident verts = makeMeshV ident (V.fromList verts)

makeMesh' :: (Storable (Vertex v)) => [Vertex v] -> Mesh v
makeMesh' verts = makeMeshV' (V.fromList verts)


makeMeshV :: (Storable (Vertex v)) => String -> V.Vector (Vertex v) -> Mesh v
makeMeshV ident verts = 
    let vec         = V.convert verts
        hash        = XH.xxHash' . BS.vectorToByteString $ vec
    in Mesh ident vec hash


makeMeshV' :: (Storable (Vertex v)) => V.Vector (Vertex v) -> Mesh v
makeMeshV' verts = 
    let vec         = V.convert verts
        hash        = XH.xxHash' . BS.vectorToByteString $ vec
    in Mesh (show hash) vec hash


emptyMesh :: (Storable (Vertex v)) => Mesh v
emptyMesh = Mesh "" VS.empty 0


updateMesh :: (Storable (Vertex v)) => Mesh v -> [Vertex v] -> Mesh v
updateMesh mesh verts =  
    let vec         = VS.fromList verts
        hash        = XH.xxHash' . BS.vectorToByteString $ vec
    in mesh{_meshData = vec, _meshHash = hash}

pushToBack :: (Storable (Vertex v)) => Mesh v -> [Vertex v] -> Mesh v
pushToBack mesh@Mesh{ _meshData, _meshHash } verts =
    let newdata = _meshData VS.++ VS.fromList verts 
        newHash = XH.xxHash' . BS.vectorToByteString $ newdata
    in mesh{ _meshData = newdata, _meshHash = newHash }

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
