{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yage.Rendering.Backend.Renderer.DeferredLighting where

import Yage.Prelude

import Control.Arrow (returnA)

import Yage.Rendering.Types
import Yage.Rendering.Backend.RenderPipeline
import Yage.Rendering.Backend.Renderer
import Yage.Rendering.Backend.Renderer.Types
import Yage.Rendering.Backend.Framebuffer




data DeferredLightingData = DeferredLightingData
    { geometries   :: [ RenderData ]
    , lights       :: [ RenderData ]
    , view         :: RenderView
    --, forwards   :: (View, [Geo])
    }


data FramebufferSetup = FramebufferSetup
    { framebuffer    :: Framebuffer
    , passShader     :: ShaderProgram
    , globalUniforms :: ShaderDefinition ()
    , globalTextures :: [TextureAssignment]
    }


instance WithSetup FramebufferSetup Renderer where
    withSetup s@FramebufferSetup{..} ma = do
        withFramebuffer framebuffer DrawTarget $ \fb ->
         withShader passShader                  $ \sh ->
          withTexturesAt Texture2D globalTextures $ do
            runUniform globalUniforms sh
            ma s


type DeferredPipeline = RenderPipeline FramebufferSetup Renderer
type GeoPass          = DeferredPipeline (RenderView, [RenderData]) Framebuffer
type LightPass        = DeferredPipeline (Framebuffer, RenderView, [RenderData]) Framebuffer


deferredShading :: GeoPass -> LightPass -> DeferredPipeline DeferredLightingData ()
deferredShading geoPass lightPass = proc (DeferredLightingData geos lights view) -> do
    gbuffer <- geoPass   -< (view, geos)
    lbuffer <- lightPass -< (gbuffer, view, lights)
    returnA -< ()





geoPass setup = 
    mkRenderPass setup $ \(view, geos) -> do
        renderFrame view geos
        return ()


lightPass setup = 
    mkRenderPass setup $ \(gbuffer, view, lights) -> do
        renderFrame view lights
        return ()


