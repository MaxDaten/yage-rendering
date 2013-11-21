module Yage.Rendering.RenderEntity where


import qualified Graphics.GLUtil.Camera3D as Cam
import           Linear                   (V3 (..), axisAngle, zero)
import           Yage.Rendering.Types

mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { _entityPosition     = zero
    , _entityOrientation  = axisAngle (V3 0 1 0) (Cam.deg2rad 0)
    , _entityScale        = V3 1 1 1
    , _entityRenderDef    = def
    }

toRenderEntity :: SomeRenderable -> RenderEntity
toRenderEntity (SomeRenderable r) = RenderEntity
    { _entityPosition    = renderPosition r
    , _entityOrientation = renderOrientation r
    , _entityScale       = renderScale r
    , _entityRenderDef   = renderDefinition r
    }
