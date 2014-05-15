{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Yage.Rendering.Backend.RenderPass where

import Yage.Prelude

import Yage.Rendering.Backend.Framebuffer
import Yage.Rendering.Backend.Renderer
import Yage.Rendering.Shader
import Yage.Rendering.Resources.ResTypes
import Yage.Rendering.Types


type MultipleRenderTargets mrt = FramebufferSpec mrt RenderTargets

type DefaultRenderTarget = DefaultFramebuffer RenderTargets

data RenderTarget ident mrt = RenderTarget ident mrt

data PassDescr ident mrt vr frameU frameT entU entT = PassDescr
    { passTarget         :: RenderTarget ident mrt
    , passShader         :: ShaderResource
    , passPerFrameData   :: ShaderData frameU frameT
    , passPerEntityData  :: RenderEntity vr entU entT -> ShaderData entU entT
    , passPreRendering   :: Renderer ()
    , passPostRendering  :: Renderer ()
    }


mkRenderPass :: ( UniformFields (Uniforms fbU), UniformFields (Uniforms entU) ) =>
             FramebufferSetup fbU -> [RenderSet entU] -> Renderer ()
mkRenderPass fboSetup rSets = withFramebufferSetup fboSetup (renderFrame rSets)


renderTargets :: PassDescr ident mrt vr frameU frameT entU entT -> mrt
renderTargets PassDescr{passTarget} = let RenderTarget _ mrt = passTarget in mrt 


defaultRenderTarget :: RenderTarget String (DefaultFramebuffer a)
defaultRenderTarget = RenderTarget "default" DefaultFramebuffer
