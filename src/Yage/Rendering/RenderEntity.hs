{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Yage.Rendering.RenderEntity where

import Yage.Prelude

import           Yage.Rendering.Types
import           Yage.Rendering.Transformation
import           Yage.Rendering.Vertex



data RenderEntity vr = RenderEntity
    { _entityTransformation :: !Transformation
    , _entityRenderDef      :: !(RenderDefinition vr)
    } deriving (Typeable)

makeLenses ''RenderEntity


mkRenderEntity :: RenderDefinition rv -> RenderEntity rv
mkRenderEntity def = RenderEntity
    { _entityTransformation = idTransformation
    , _entityRenderDef    = def
    }

toRenderEntity :: (Renderable r rv) => r -> RenderEntity rv
toRenderEntity r = RenderEntity
    { _entityTransformation    = renderTransformation r
    , _entityRenderDef         = renderDefinition r
    }


instance (ViableVertex (Vertex vr)) => Renderable (RenderEntity vr) vr where
    renderDefinition        = _entityRenderDef
    renderTransformation    = _entityTransformation


--instance (RealFloat a, Typeable a) => Renderable (Viewport a) (P3 "pos" GLfloat) where
--    renderDefinition _      =
--        let quadVerts = (vertices . triangles $ quad 1)
--        in RenderDefinition
--            { _rdefData     = makeMesh "screen" quadVerts
--            --, _rdefProgram  = (shader, shdef)
--            , _rdefTextures = []
--            , _rdefMode     = Triangles
--            }
--    renderTransformation vp = 
--        let dim  = realToFrac <$> vp^.vpSize
--        in idTransformation & transPosition .~ 0
--                            & transScale    .~ V3 ( dim^._x ) ( dim^._y ) (1)