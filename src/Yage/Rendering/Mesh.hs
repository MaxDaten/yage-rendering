{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE FlexibleContexts            #-}

module Yage.Rendering.Mesh
  ( module Yage.Rendering.Mesh
  , module E
  ) where

import           Yage.Prelude                        hiding (Index, toList)
import           Yage.Lens

import qualified Data.Vector.Storable                as V
import qualified Data.Vector                         as V' (convert, (!))
import           Data.Foldable                       (toList)
--import qualified Data.Vector                         as V
import qualified Data.Digest.XXHash                  as XH
import qualified Data.Vector.Storable.ByteString     as BS

import Yage.Geometry.Vertex
import Yage.Geometry
import Yage.Geometry.Elements as E (Triangle(..))
---------------------------------------------------------------------------------------------------



type MeshHash = XH.XXHash
type MeshId   = String

data Mesh vertex = Mesh
    { _meshId         :: !MeshId
    , _meshVertices   :: !(V.Vector (Vertex vertex))
    , _meshElemCount  :: !Int
    , _meshHash       :: !MeshHash
    }
    deriving ( Typeable, Generic )

makeLenses ''Mesh



instance Show (Mesh v) where
    show Mesh{..} =
        format "Mesh {id = {0}, attribs = N/A, meshHash = {1}}"
               [show _meshId, show _meshHash]



instance Eq (Mesh v) where
    a == b = _meshId a == _meshId b



instance Ord (Mesh v) where
    compare a b = compare (_meshId a) (_meshId b)



vertexCount :: (Storable (Vertex v)) => Mesh v -> Int
vertexCount Mesh{_meshVertices} = V.length _meshVertices



elementCount :: Mesh v -> Int
elementCount = _meshElemCount

---------------------------------------------------------------------------------------------------


-- | constructs a mehs with given identifier and inital vertices, hash is calculated on vertices
makeMesh :: (Storable (Vertex v)) => MeshId -> V.Vector (Vertex v) -> Mesh v
makeMesh ident verts = 
    let cnt         = V.length verts `div` 3
        hash        = XH.xxHash' $ (BS.vectorToByteString verts)
    in Mesh ident verts cnt hash


meshFromVertexList :: (Storable (Vertex v)) => MeshId -> [Vertex v] -> Mesh v
meshFromVertexList ident = makeMesh ident . V.fromList


-- | builds a mesh from geometry, unrolling the elements (duplicating vertices)
-- for a index free drawing
meshFromTriGeo :: (Storable (Vertex v)) =>
                         MeshId -> TriGeo (Vertex v) -> Mesh v
meshFromTriGeo ident Geometry{..} = 
  makeMesh ident $
    V.concatMap (V.fromList . map (geoVertices V'.!) . toList) $ V'.convert geoElements


-- | unified empty mesh with "" identifier
emptyMesh :: (Storable (Vertex v)) => Mesh v
emptyMesh = Mesh "" V.empty 0 0


-- | replaces vertices in Mesh, keeps ident, recalucates hash
updateMesh :: (Storable (Vertex v)) => Mesh v -> V.Vector (Vertex v) -> Mesh v
updateMesh Mesh{_meshId} = makeMesh _meshId


-- | appends vertices to mesh, keeps ident, hash is recalculated
pushToBack :: (Storable (Vertex v)) => Mesh v -> V.Vector (Vertex v) -> Mesh v
pushToBack Mesh{ _meshId, _meshVertices } verts =
  makeMesh _meshId (_meshVertices V.++ verts)
