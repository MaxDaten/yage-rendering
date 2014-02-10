{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Yage.Rendering.RenderEntity where

import Yage.Prelude

import           Yage.Rendering.Types
import           Yage.Rendering.Lenses
import           Yage.Rendering.Primitives
import           Yage.Rendering.VertexSpec
import           Yage.Rendering.Mesh

import           Linear                     (V2(..), V3(..), _x, _y)


mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { _entityTransformation = idTransformation
    , _entityRenderDef    = def
    }

toRenderEntity :: (Renderable r) => r -> RenderEntity
toRenderEntity r = RenderEntity
    { _entityTransformation    = renderTransformation r
    , _entityRenderDef         = renderDefinition r
    }


instance Renderable RenderEntity where
    renderDefinition        = _entityRenderDef
    renderTransformation    = _entityTransformation


instance (RealFloat a, Typeable a) => Renderable (Viewport a) where
    renderDefinition _      =
        let shader    = ShaderResource "nooooo" "noootooo"
            shdef     = return ()
            mesh      = quadMesh $ V2 1 1
            attribs   = \m ->
                        [ "in_vert_position" @= m^.mDataVertices^..traverse.vPosition
                        , "in_vert_texture"  @= m^.mDataVertices^..traverse.vTexture
                        ]
        in RenderDefinition
            { _rdefData     = makeMesh 0 "screen" mesh attribs
            , _rdefProgram  = (shader, shdef)
            , _rdefTextures = []
            , _rdefMode     = Triangles
            }
    renderTransformation vp = 
        let dim  = {-- realToFrac (vp^.vpFactor) * --} (realToFrac <$> vp^.vpSize)
        in idTransformation & transPosition .~ 0 -- V3 ( xy^._x ) ( xy^._y ) (0)
                            & transScale    .~ V3 ( dim^._x ) ( dim^._y ) (1)