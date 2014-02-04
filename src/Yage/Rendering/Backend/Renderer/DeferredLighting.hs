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
    }


data FramebufferSetup = FramebufferSetup
    { framebuffer    :: Framebuffer
    , passShader     :: ShaderProgram
    , globalUniforms :: ShaderDefinition ()
    --, globalTextures :: [TextureAssignment]
    , preRendering   :: Renderer ()
    , postRendering  :: Renderer ()
    }


instance WithSetup FramebufferSetup Renderer where
    withSetup FramebufferSetup{..} ma = do
        withFramebuffer framebuffer DrawTarget $ \_fb ->
         withShader passShader                  $ \sh -> do
          --withTexturesAt Texture2D globalTextures $ do
            preRendering
            runUniform globalUniforms sh
            r <- ma
            postRendering
            return r


type DeferredPipeline = RenderPipeline FramebufferSetup Renderer
type GeoPass          = DeferredPipeline ([RenderData]) Framebuffer
type LightPass        = DeferredPipeline (Framebuffer, [RenderData]) Framebuffer


deferredLighting :: GeoPass -> LightPass -> DeferredPipeline DeferredLightingData ()
deferredLighting geoPass lightPass = proc (DeferredLightingData geos lights) -> do
    gbuffer <- geoPass   -< geos
    lbuffer <- lightPass -< (gbuffer, lights)
    returnA -< ()




mkGeoPass :: FramebufferSetup -> GeoPass
mkGeoPass setup = 
    mkRenderPass setup $ \geos -> do
        renderFrame geos
        return $ framebuffer setup

mkLightPass :: FramebufferSetup -> LightPass
mkLightPass setup = 
    mkRenderPass setup $ \(gbuffer, lights) -> do
        renderFrame lights
        return $ framebuffer setup


