{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Yage.Rendering
    ( module Types
    , module Yage.Rendering
    , module Lenses
    
    , module Mesh
    , module Vertex
    , module Uniforms
    
    , module LinExport
    , module Framebuffer
    , module ResourceManager
    , module Pass
    ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math

import           Control.Monad.RWS                      hiding (forM)

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.RenderPass      as Pass
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as ResourceManager (GLResources, initialGLRenderResources)
import           Yage.Rendering.Types                   as Types

import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.Uniforms                as Uniforms
import           Yage.Rendering.Vertex                  as Vertex


type RenderSystem = RWST () RenderLog GLResources IO



runRenderSystem :: (MonadIO m) => RenderSystem () -> GLResources -> m (GLResources, RenderLog)
runRenderSystem sys res = io $ execRWST sys () res


-- TODO individual settings
mkRenderSystem :: Renderer () -> RenderSystem ()
mkRenderSystem toRender = do
    (_, rlog) <- io $ Renderer.runRenderer toRender
    tell rlog
{--
outerRim = flip runRenderSystem res $ do
    runRenderPass geoDescr geos
    runRenderPass lightPass lights
    runRenderPass geoDescr screen
--}

runRenderPass :: (Renderable ent vr, UniformFields (Uniforms globals), UniformFields (Uniforms locals) ) 
              => PassDescr ent globals locals -> [ent] -> RenderSystem ()
runRenderPass passDescr@PassDescr{..} entities = do
    (setup, rSets) <- managePassResoures
    mkRenderSystem $ mkRenderPass setup rSets

    where
    managePassResoures = do
        res <- get 
        (results, res', reslog) <- runResourceManager res $ 
            (,) <$> (requestFramebufferSetup passDescr)
                <*> (forM entities $ \e -> requestRenderSet passShader (passEntityUniforms e) (renderDefinition e))
        scribe resourceLog reslog
        put res'
        return results

