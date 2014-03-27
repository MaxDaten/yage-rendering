{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Yage.Rendering.Backend.RenderPass where

import Yage.Prelude

import Yage.Rendering.Backend.Framebuffer
import Yage.Rendering.Backend.Renderer
import Yage.Rendering.Uniforms
import Yage.Rendering.Types


type MultipleRenderTargets mrt = FramebufferSpec mrt RenderTargets

type DefaultRenderTarget = DefaultFramebuffer RenderTargets

data RenderTarget ident mrt = RenderTarget ident mrt

data PassDescr ident mrt ent global local = PassDescr
    { passTarget         :: RenderTarget ident mrt
    , passShader         :: ShaderResource
    , passGlobalUniforms :: Uniforms global
    , passEntityUniforms :: ent -> Uniforms local
    , passGlobalTextures :: [TextureDefinition]
    , passPreRendering   :: Renderer ()
    , passPostRendering  :: Renderer ()
    }


mkRenderPass :: ( UniformFields (Uniforms globalU), UniformFields (Uniforms localU) )
              => FramebufferSetup globalU -> [RenderSet localU] -> Renderer ()
mkRenderPass fboSetup rSets = withFramebufferSetup fboSetup (renderFrame rSets)

renderTargets :: PassDescr ident mrt ent global local -> mrt
renderTargets PassDescr{passTarget} = let RenderTarget _ mrt = passTarget in mrt 

defaultRenderTarget :: RenderTarget String (DefaultFramebuffer a)
defaultRenderTarget = RenderTarget "default" DefaultFramebuffer
