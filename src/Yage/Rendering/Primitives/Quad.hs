{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Yage.Rendering.Primitives.Quad where

import Yage.Prelude

import Yage.Rendering.Types
import Yage.Rendering.VertexSpec
import Yage.Rendering.Primitives.Basic
import Yage.Math

---------------------------------------------------------------------------------------------------
-- Primitives


quadMesh :: Real a => V2 a -> MeshData Vertex3P3N3C4T2
quadMesh dim = 
    let V2 x y = (realToFrac <$> dim) / 2.0
        tl    = (V3 (-x)   y  0.0, uv01)
        tr    = (V3   x    y  0.0, uv11)
        br    = (V3   x  (-y) 0.0, uv10)
        bl    = (V3 (-x) (-y) 0.0, uv00)
        verts = [tl, bl, br, tr]
        ixs   = [ 0, 1, 2
                , 2, 3, 0
                ]
    in makeMeshfromSpare verts ixs white

