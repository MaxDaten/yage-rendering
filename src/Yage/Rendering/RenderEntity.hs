{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yage.Rendering.RenderEntity where

--import Yage.Prelude

--import qualified Graphics.GLUtil.Camera3D as Cam
--import           Linear                   (V3 (..), axisAngle, zero)
import           Yage.Rendering.Types

mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { _entityTransformation = idTransformation
    , _entityRenderDef    = def
    }

toRenderEntity :: SomeRenderable -> RenderEntity
toRenderEntity (SomeRenderable r) = RenderEntity
    { _entityTransformation    = renderTransformation r
    , _entityRenderDef         = renderDefinition r
    }


instance Renderable RenderEntity where
    renderDefinition        = _entityRenderDef
    renderTransformation    = _entityTransformation
