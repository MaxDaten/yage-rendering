{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Rendering.Mesh where

import           Yage.Prelude
import           Data.List (length)
import           Foreign                             (Storable)


import Yage.Rendering.Types
import Yage.Rendering.VertexSpec



makeMeshFromVerts :: (Storable v) => Int -> String -> [v] -> [Index] -> [VertexAttribute] -> Mesh
-- TODO some assertions for invalid meshes
makeMeshFromVerts ident name vs ixs attribs =
    let meshdata = MeshData vs ixs $ length ixs `quot` 3
    in Mesh ident name meshdata attribs True

makeMesh :: (Storable v) => Int -> String -> MeshData v -> [VertexAttribute] -> Mesh
makeMesh ident name meshdata attribs = Mesh ident name meshdata attribs True

emptyMeshData :: MeshData v
emptyMeshData = MeshData [] [] 0

meshTriangleCount :: Mesh -> Int
meshTriangleCount (Mesh _ _ (MeshData _ _ cnt) _ _) = cnt