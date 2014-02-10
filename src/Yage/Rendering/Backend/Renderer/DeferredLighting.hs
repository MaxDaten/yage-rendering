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
import Yage.Rendering.ResourceManager.Types

data DeferredLightingData = DeferredLightingData
    { geometries   :: [ RenderData ]
    , lights       :: [ RenderData ]
    , screen       :: RenderData
    }


data FramebufferSetup = FramebufferSetup
    { framebuffer       :: GLFramebuffer
    , fbShader          :: ShaderProgram
    , fbGlobalUniforms  :: ShaderDefinition ()
    , globalTextures    :: [TextureAssignment]
    , preRendering      :: Renderer ()
    , postRendering     :: Renderer ()
    }


instance WithSetup FramebufferSetup Renderer where
    withSetup FramebufferSetup{..} ma = do
        withFramebuffer framebuffer DrawTarget $ \_fb ->
         withShader fbShader                  $ \sh -> do
          withTexturesAt Texture2D globalTextures $ do
            preRendering
            runUniform fbGlobalUniforms sh
            r <- ma
            postRendering
            return r


type DeferredPipeline = RenderPipeline FramebufferSetup Renderer
type GeoPass          = DeferredPipeline ([RenderData]) GLFramebuffer
type LightPass        = DeferredPipeline (GLFramebuffer, [RenderData]) GLFramebuffer
type ScreenPass       = DeferredPipeline (GLFramebuffer, RenderData) ()


deferredLighting :: GeoPass -> ScreenPass -> DeferredPipeline DeferredLightingData ()
deferredLighting geoPass {--lightPass--} screenPass = proc (DeferredLightingData geos lights screen) -> do
    gbuffer <- geoPass    -< geos
    --lbuffer <- lightPass  -< (gbuffer, lights)
    fin     <- screenPass -< (gbuffer, screen)
    returnA -< fin




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

mkScreenPass :: FramebufferSetup -> ScreenPass
mkScreenPass setup =
    mkRenderPass setup $ \(buff, screen) -> do
        renderFrame [screen]
        return ()
