{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
module Yage.Rendering.Backend.RenderPass where

import Yage.Prelude

import Yage.Rendering.Backend.Framebuffer
import Yage.Rendering.Backend.Renderer
import Yage.Rendering.Uniforms
import Yage.Rendering.Types


data TargetFramebuffer =
      DefaultFramebuffer 
    | CustomFramebuffer (String, FramebufferSpec TextureResource RenderbufferResource)

data PassDescr ent global local = PassDescr
    { passFBSpec         :: TargetFramebuffer
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


