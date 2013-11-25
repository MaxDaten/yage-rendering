{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Rendering.Mesh where

import           Yage.Prelude                        hiding (Index)
import           Data.List (length)
import           Foreign                             (Storable)


import Yage.Rendering.Types
import Yage.Rendering.VertexSpec


type GetVertexAttributes v = (MeshData v -> [VertexAttribute])

makeMeshFromVerts :: (Storable v) => Int -> String -> [v] -> [Index] -> GetVertexAttributes v -> Mesh
-- TODO some assertions for invalid meshes
makeMeshFromVerts ident name vs ixs attribs =
    let meshdata = MeshData vs ixs $ length ixs `quot` 3
    in Mesh ident name meshdata attribs True

makeMesh :: (Storable v) => Int -> String -> MeshData v -> GetVertexAttributes v -> Mesh
makeMesh ident name meshdata getAttribs = Mesh ident name meshdata getAttribs True

emptyMeshData :: MeshData v
emptyMeshData = MeshData [] [] 0

meshTriangleCount :: Mesh -> Int
meshTriangleCount (Mesh _ _ (MeshData _ _ cnt) _ _) = cnt