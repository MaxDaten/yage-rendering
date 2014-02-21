{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Yage.Rendering.Backend.Renderer.DeferredLighting where

import Yage.Prelude

import Yage.Rendering.Types
import Yage.Rendering.Backend.RenderPipeline
import Yage.Rendering.Backend.Renderer
import Yage.Rendering.Backend.Framebuffer
import Yage.Rendering.Resources.Types
import Yage.Rendering.Uniforms

type HasPassUniforms global local = (UniformFields (Uniforms global), UniformFields (Uniforms local))


data DeferredLightingData gu lu su = DeferredLightingData
    { geometries   :: [ RenderSet gu ]
    , lights       :: [ RenderSet lu ]
    , screen       :: RenderSet su
    }



data FramebufferSetup u = FramebufferSetup
    { framebuffer       :: GLFramebuffer
    , fbShader          :: ShaderProgram
    , fbGlobalUniforms  :: Uniforms u
    , globalTextures    :: [TextureAssignment]
    , preRendering      :: Renderer ()
    , postRendering     :: Renderer ()
    }


instance UniformFields (Uniforms u) => WithSetup (FramebufferSetup u) Renderer where
    withSetup FramebufferSetup{..} ma = do
        withFramebuffer framebuffer DrawTarget $ \_fb ->
         withShader fbShader                   $ \sh -> do
          withTexturesAt Texture2D globalTextures $ do
            preRendering
            io $ setUniforms sh fbGlobalUniforms
            r <- ma
            postRendering
            return r

type DeferredPipeline    = RenderPipeline () Renderer
type DeferredPass ur     = RenderPipeline (FramebufferSetup ur) Renderer
type GeoPass ur gr       = DeferredPass ur ([ RenderSet gr ]) GLFramebuffer
type LightPass ur lr     = DeferredPass ur (GLFramebuffer, [ RenderSet lr ]) GLFramebuffer
type ScreenPass ur sr    = DeferredPass ur (GLFramebuffer, RenderSet sr) ()



deferredLighting :: (HasPassUniforms gGlo gLoc, HasPassUniforms sGlo sLoc) 
                 => GeoPass gGlo gLoc -> ScreenPass sGlo sLoc -> DeferredLightingData gLoc lLoc sLoc -> Renderer ()
deferredLighting geoPass screenPass (DeferredLightingData geos _lights screen) = do
    gbuffer <- runPipeline geoPass geos
    --lbuffer <- lightPass  -< (gbuffer, lights)
    fin     <- runPipeline screenPass (gbuffer, screen)
    return fin



mkGeoPass :: (HasPassUniforms global local) 
          => FramebufferSetup global -> GeoPass global local
mkGeoPass setup = 
    mkRenderPass setup $ \geos -> do
        renderFrame geos
        return $ framebuffer setup

mkLightPass :: (HasPassUniforms global local) 
            => FramebufferSetup global -> LightPass global local
mkLightPass setup = 
    mkRenderPass setup $ \(_gbuffer, lights) -> do
        renderFrame lights
        return $ framebuffer setup

mkScreenPass :: (HasPassUniforms global local) 
             => FramebufferSetup global -> ScreenPass global local
mkScreenPass setup =
    mkRenderPass setup $ \(_buff, screen) -> do
        renderFrame [screen]
        return ()
