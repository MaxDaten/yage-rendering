{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Yage.Rendering.Primitives
  (cubeMesh, quadMesh
  , makeMeshfromSpare, processSpareVerts
  ) where

import Yage.Prelude -- hiding (id)
import Control.Lens hiding (Index, indices)

import Data.List ((++), map, take, length, (!!), repeat, zipWith4, unzip)
import Data.Foldable

import Linear (V3(..), V4(..), R3(_xyz), point)

import Yage.Rendering.Types
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

type Face = [(Position3f, Color4f, Texture2f)]

data SimpleCubeDef f = SimpleCubeDef
  { front  :: f
  , left   :: f
  , right  :: f
  , top    :: f
  , bottom :: f
  , hidden :: f
  } deriving (Typeable, Functor, Foldable)

defaultCubeDef :: SimpleCubeDef Face
defaultCubeDef = SimpleCubeDef
    { front   = [(tlf, white, uv01), (blf, white, uv00), (brf, white, uv10), (trf, white, uv11)]
    , left    = [(tlh, white, uv01), (blh, white, uv00), (blf, white, uv10), (tlf, white, uv11)]
    , right   = [(trf, white, uv01), (brf, white, uv00), (brh, white, uv10), (trh, white, uv11)]
    , top     = [(tlh, white, uv01), (tlf, white, uv00), (trf, white, uv10), (trh, white, uv11)]
    , bottom  = [(blf, white, uv01), (blh, white, uv00), (brh, white, uv10), (brf, white, uv11)]
    , hidden  = [(trh, white, uv01), (brh, white, uv00), (blh, white, uv10), (tlh, white, uv11)]
    }

emptyMesh = Mesh
  { ident    = "empytMesh"
  , vertices = []
  , indices  = []
  , triCount = 0
  }

cubeMesh' :: SimpleCubeDef Face -> Mesh Vertex4342
cubeMesh' def@SimpleCubeDef{..} = (foldr addFaceToMesh emptyMesh def){ ident = "cubeMesh" }

cubeMesh :: Mesh Vertex4342
cubeMesh = cubeMesh' defaultCubeDef



quadMesh :: Mesh Vertex4342
quadMesh = 
    let tl    = (V3 (-1.0)   1.0  0.0, uv10)
        tr    = (V3   1.0    1.0  0.0, uv11)
        br    = (V3   1.0  (-1.0) 0.0, uv01)
        bl    = (V3 (-1.0) (-1.0) 0.0, uv00)
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


addFaceToMesh :: Face -> Mesh Vertex4342 -> Mesh Vertex4342
addFaceToMesh face@(v0:v1:v2:_:[]) mesh@Mesh{..} = 
  let (normal, _, _)  = plainNormalForm (v2^._1) (v1^._1) (v0^._1)
  in mesh 
        { vertices  = map (\(p, c, t) -> Vertex (point p) normal c t) face ++ vertices 
        , indices   = [0, 1, 2, 2, 3, 0] ++ map (+4) indices
        , triCount  = triCount + 2
        }
addFaceToMesh _ _     = error "invalid face" 
